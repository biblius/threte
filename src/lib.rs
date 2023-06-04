pub mod display;
pub mod id;
pub mod item;
pub mod node;

use crate::{
    item::{AlphaMemoryItem, ConditionType},
    node::{BetaMemoryNode, JoinNode, ProductionNode},
};
use item::{Condition, ConstantTest, NegativeJoinResult, Production, TestAtJoinNode, Token, Wme};
use node::{AlphaMemoryNode, NegativeNode, Node};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

type RcCell<T> = Rc<RefCell<T>>;

trait IntoCell: Sized {
    fn to_cell(self) -> RcCell<Self> {
        Rc::new(RefCell::new(self))
    }
}

trait IntoNodeCell: Sized {
    fn to_node_cell(self) -> RcCell<Node>;
}

#[derive(Debug)]
pub struct Rete {
    /// Token tree root
    pub dummy_top_token: RcCell<Token>,

    /// Beta network root
    pub dummy_top_node: RcCell<Node>,

    /// Since every WME is represented as a triple, we only need to do 8 hash table look ups whenever one is added to the
    /// network to find possibly matching constant tests for it. This removes the need for a constant test network.
    pub constant_tests: HashMap<ConstantTest, RcCell<AlphaMemoryNode>>,

    /// Maps WME IDs to their corresponding elements for quick removal of tokens
    pub working_memory: HashMap<usize, RcCell<Wme>>,

    /// Maps WME IDs to Alpha Nodes that contain items which hold the WME
    pub wme_alphas: HashMap<usize, Vec<RcCell<AlphaMemoryNode>>>,

    /// Maps production IDs to their corresponding production nodes
    pub productions: HashMap<usize, RcCell<Node>>,
}

impl Default for Rete {
    fn default() -> Self {
        Self::new()
    }
}

impl Rete {
    fn new() -> Self {
        let dummy_top_node = BetaMemoryNode::new(None);
        println!("Created initial dummy {dummy_top_node}");
        let dummy_top_node = dummy_top_node.to_node_cell();

        let dummy_top_token = Token::new(&dummy_top_node, None, None);

        dummy_top_node.borrow_mut().add_token(&dummy_top_token);

        Self {
            constant_tests: HashMap::new(),
            wme_alphas: HashMap::new(),
            working_memory: HashMap::new(),
            productions: HashMap::new(),
            dummy_top_node,
            dummy_top_token,
        }
    }

    pub fn add_wme(&mut self, elements: [usize; 3]) -> usize {
        let wme = Wme::new(elements);
        let id = wme.id;

        println!("Adding WME {:?}", wme);

        for element in wme.permutations() {
            let Some(memory) = self.constant_tests.get(&element) else { continue };

            // Index the memories that will hold this WME by its ID
            if let Some(memories) = self.wme_alphas.get_mut(&id) {
                memories.push(Rc::clone(memory));
            } else {
                self.wme_alphas.insert(id, vec![Rc::clone(memory)]);
            }

            let wme = wme.to_cell();
            self.working_memory.insert(id, Rc::clone(&wme));

            println!(
                "Found existing memory {} for element {:?}",
                memory.borrow().id,
                element
            );

            activate_alpha_memory(memory, &wme);

            return id;
        }

        // Ensure the WME has a corresponding Alpha Node in advance if none is found
        println!("No memory found for WME {wme:?}");

        let wme = wme.to_cell();
        self.working_memory.insert(id, Rc::clone(&wme));

        id
    }

    pub fn remove_wme(&mut self, id: usize) {
        println!("Removing WME {id}");

        // Remove all items representing the wme from the alpha network
        if let Some(memories) = self.wme_alphas.remove(&id) {
            for memory in memories {
                println!("Removing WME {id} from alpha memory {}", memory.borrow().id);
                memory
                    .borrow_mut()
                    .items
                    .retain(|item| item.borrow().wme.borrow().id != id);
            }
        }

        if let Some(wme) = self.working_memory.remove(&id) {
            println!("Removing WME {} from working memory", wme.borrow());

            let mut wme = wme.borrow_mut();
            let mut tokens = std::mem::take(&mut wme.tokens);
            let n_join_results = std::mem::take(&mut wme.negative_join_results);

            drop(wme);

            // Remove all tokens representing the wme
            while let Some(token) = tokens.pop() {
                Token::delete_self_and_descendants(token)
            }

            // Remove all associated negative join results from the result's owner
            // and trigger left activation to test for new absence
            for result in n_join_results {
                let result = result.borrow();

                result
                    .owner
                    .borrow_mut()
                    .negative_join_results
                    .retain(|res| res.borrow().id != result.id);

                if result.owner.borrow().negative_join_results.is_empty() {
                    if let Some(children) = result.owner.borrow().node.borrow().children() {
                        for child in children {
                            activate_left(child, &result.owner, None);
                        }
                    }
                }
            }
        }
    }

    pub fn add_production(&mut self, production: Production) -> usize {
        println!("Adding production {}", production.id);

        let id = production.id;
        let conditions = &production.conditions;

        assert!(!conditions.is_empty(), "LHS of production cannot be empty");

        println!("Processing condition {:?}", conditions[0]);

        let mut current_node = Rc::clone(&self.dummy_top_node);

        let mut earlier_conds = vec![];

        let mut tests = get_join_tests_from_condition(&conditions[0], &earlier_conds);
        let mut alpha_memory = self.build_or_share_alpha_memory_node(&conditions[0]);
        current_node = build_or_share_join_node(&current_node, &alpha_memory, tests);

        for i in 1..conditions.len() {
            let condition = &conditions[i];
            println!("Processing condition {:?}", condition);
            match condition._type() {
                ConditionType::Negative => {
                    tests = get_join_tests_from_condition(condition, &earlier_conds);
                    alpha_memory = self.build_or_share_alpha_memory_node(condition);
                    current_node =
                        build_or_share_negative_node(&current_node, &alpha_memory, tests);
                }
                ConditionType::Positive => {
                    current_node = build_or_share_beta_memory_node(&current_node);
                    tests = get_join_tests_from_condition(condition, &earlier_conds);
                    alpha_memory = self.build_or_share_alpha_memory_node(condition);
                    current_node = build_or_share_join_node(&current_node, &alpha_memory, tests);
                }
                ConditionType::NegativeConjunction => todo!(),
            }
            earlier_conds.push(conditions[i - 1]);
        }

        let production = ProductionNode::new(production, &current_node).to_node_cell();

        self.productions.insert(id, Rc::clone(&production));

        current_node.borrow_mut().add_child(&production);

        update_new_node_with_matches_from_above(&production);

        id
    }

    pub fn remove_production(&mut self, id: usize) -> bool {
        let Some(production) = self.productions.remove(&id) else { return false; };

        println!("Removing production {}", production.borrow());

        let constant_tests = match &*production.borrow() {
            Node::Production(prod) => prod
                .production
                .conditions
                .iter()
                .map(|cond| ConstantTest::from(*cond))
                .collect::<Vec<_>>(),
            _ => unreachable!(),
        };

        //TODO handle negative nodes

        self.delete_node_and_unused_ancestors(production, &constant_tests);

        true
    }

    fn build_or_share_alpha_memory_node(
        &mut self,
        condition: &Condition,
    ) -> RcCell<AlphaMemoryNode> {
        let constant_test = ConstantTest::from(*condition);

        println!("Searching for constant test {constant_test:?}");

        // Check whether an alpha memory like this exists
        if let Some(alpha_mem) = self.constant_tests.get(&constant_test) {
            println!("Shared {}", alpha_mem.borrow());
            return Rc::clone(alpha_mem);
        }

        // Alpha memory not found, create new one and insert into map
        let am = AlphaMemoryNode::new().to_cell();

        self.constant_tests.insert(constant_test, Rc::clone(&am));

        println!(
            "Inserting constant test {:?} for AM {}",
            constant_test,
            am.borrow().id
        );

        for (_, wme) in self.working_memory.iter() {
            let _wme = wme.borrow();
            if constant_test.matches(&_wme) {
                let a_mems = self
                    .wme_alphas
                    .entry(_wme.id)
                    .or_insert_with(|| vec![Rc::clone(&am)]);
                a_mems
                    .iter()
                    .for_each(|mem| activate_alpha_memory(mem, wme))
            }
        }

        am
    }

    fn delete_node_and_unused_ancestors(
        &mut self,
        node: RcCell<Node>,
        constant_tests: &[ConstantTest],
    ) {
        println!("Deleting Node {}", node.borrow());
        // Done to avoid the mutable reference
        let (alpha_successors, tokens) = match &mut *node.borrow_mut() {
            Node::Join(ref mut join) => {
                let (mem, successors) = {
                    let mut alpha_mem = join.alpha_memory.borrow_mut();
                    (
                        Rc::clone(&join.alpha_memory),
                        std::mem::take(&mut alpha_mem.successors),
                    )
                };

                (Some((mem, successors)), None)
            }
            Node::Beta(beta) => {
                let tokens = std::mem::take(&mut beta.items);
                (None, Some(tokens))
            }
            Node::Production(_) => (None, None),
            Node::Negative(_) => (None, None), // TODO
        };

        // Node is a join node, clear all alpha mems related to production
        if let Some((alpha_mem, successors)) = alpha_successors {
            println!("Deleting Alpha Mem {}", alpha_mem.borrow());
            if successors.is_empty() {
                alpha_mem.borrow_mut().items.clear();
            }
            for test in constant_tests {
                let Some(mem) = self.constant_tests.get(test) else { continue; };
                if mem.borrow().successors.is_empty() {
                    self.constant_tests.remove(test);
                }
            }
        }

        // Node is a beta node, clear all tokens related to production
        if let Some(mut tokens) = tokens {
            while let Some(token) = tokens.pop() {
                Token::delete_self_and_descendants(token)
            }
        }

        if let Some(parent) = node.borrow().parent() {
            {
                parent.borrow_mut().remove_child(node.borrow().id());
            }
            if parent.borrow().children().is_none() {
                self.delete_node_and_unused_ancestors(parent, constant_tests);
            }
        }
    }
}

fn update_new_node_with_matches_from_above(node: &RcCell<Node>) {
    println!("Updating node {}", node.borrow());

    let Some(parent) = node.borrow().parent() else { return; };

    println!("Updating parent {}", parent.borrow());

    let children = match *parent.borrow_mut() {
        Node::Beta(ref beta) => {
            beta.items
                .iter()
                .for_each(|token| activate_left(node, token, token.borrow().wme.as_ref()));
            return;
        }
        Node::Join(ref mut join) => std::mem::replace(&mut join.children, vec![Rc::clone(node)]),
        Node::Production(_) => panic!("Production node cannot have children"),
        Node::Negative(_) => todo!(),
    };

    // This avoids keeping the mutable borrow when recursively activating
    {
        let Node::Join(ref join) = *parent.borrow() else { return; };
        join.alpha_memory
            .borrow()
            .items
            .iter()
            .for_each(|item| activate_right(&parent, &item.borrow().wme));
    }

    let Node::Join(ref mut join) = *parent.borrow_mut() else { return; };
    join.children = children;
}

/// Activation of alpha memories cause them to right activate join nodes which in turn makes
/// the join nodes search through their beta memories and perform matches on already existing tokens,
/// further propagating left activations if they succeed.
fn activate_alpha_memory(alpha_mem_node: &RcCell<AlphaMemoryNode>, wme: &RcCell<Wme>) {
    let item = AlphaMemoryItem::new(wme, alpha_mem_node).to_cell();

    // Insert new item at the head of the node's items
    let mut alpha_mem = alpha_mem_node.borrow_mut();
    alpha_mem.items.push(Rc::clone(&item));

    println!("Activating Alpha Node: {alpha_mem}");

    alpha_mem
        .successors
        .iter()
        .for_each(|node| activate_right(node, wme))
}

/// Left activation of Beta nodes cause them to create tokens and propagate the left activation to their children, i.e.
/// Join nodes, with the newly created token.
///
/// Left activation of Join nodes cause them to execute their join tests with the given token and if successful propagate
/// the left activation to their children.
///
/// Left activation of production nodes cause them to activate the underlying production.
fn activate_left(node: &RcCell<Node>, parent_token: &RcCell<Token>, wme: Option<&RcCell<Wme>>) {
    match &mut *node.borrow_mut() {
        Node::Beta(ref mut beta_node) => {
            let new_token = Token::new(node, Some(parent_token), wme);
            println!(
                "Left activating beta {} and appending token {}",
                beta_node.id,
                new_token.borrow().id
            );
            beta_node.items.push(Rc::clone(&new_token));
            beta_node
                .children
                .iter()
                .for_each(|child| activate_left(child, &new_token, wme));
        }
        Node::Join(ref mut join_node) => {
            println!("Left activating join {}", join_node.id);

            for alpha_mem_item in join_node.alpha_memory.borrow().items.iter() {
                let test = join_test(
                    &join_node.tests,
                    parent_token,
                    &alpha_mem_item.borrow().wme.borrow(),
                );
                if test {
                    join_node.children.iter().for_each(|child| {
                        activate_left(child, parent_token, Some(&alpha_mem_item.borrow().wme))
                    });
                }
            }
        }
        Node::Negative(negative_node) => {
            println!("Left activating negative {}", negative_node.id);

            let new_token = Token::new(node, Some(parent_token), wme);

            negative_node.items.push(Rc::clone(&new_token));

            for item in negative_node.alpha_mem.borrow().items.iter() {
                let test = join_test(
                    &negative_node.tests,
                    &new_token,
                    &item.borrow().wme.borrow(),
                );
                if test {
                    let join_result =
                        NegativeJoinResult::new(&new_token, &item.borrow().wme).to_cell();

                    new_token
                        .borrow_mut()
                        .negative_join_results
                        .push(Rc::clone(&join_result));

                    if let Some(wme) = wme {
                        wme.borrow_mut()
                            .negative_join_results
                            .push(Rc::clone(&join_result));
                    }
                }
            }

            // Negative nodes propagate left activations only if no tokens passed its join tests
            if new_token.borrow().negative_join_results.is_empty() {
                for child in negative_node.children.iter() {
                    activate_left(child, &new_token, wme)
                }
            }
        }
        Node::Production(p_node) => {
            println!(
                "====================\nProduction node activated! {p_node}\n===================="
            )
        }
    }
}

/// A right activation of a `JoinNode` will cause it to iterate through its
/// parent's tokens and perform a [join_test] on each one and the given WME.
///
/// For every test that passes, a left activation is triggered on each of the node's
/// children.
///
/// Right activations are caused by [AlphaMemoryNode]s when [WME][Wme]s are changed or
/// when new [WME][Wme]s enter the network
fn activate_right(node: &RcCell<Node>, wme: &RcCell<Wme>) {
    let node = &*node.borrow();
    println!("Right activating {} {}", node._type(), node.id());
    match node {
        Node::Join(ref join_node) => {
            if let Node::Beta(ref parent) = *join_node.parent.borrow() {
                for token in parent.items.iter() {
                    let test = join_test(&join_node.tests, token, &wme.borrow());
                    if test {
                        join_node
                            .children
                            .iter()
                            .for_each(|child| activate_left(child, token, Some(wme)))
                    }
                }
            }
        }
        Node::Negative(ref negative_node) => {
            for token in negative_node.items.iter() {
                let test = join_test(&negative_node.tests, token, &wme.borrow());
                if test {
                    // This check is done to see if the token's state has changed. If it previously
                    // had negative join results and is now empty, it means we must delete all its children.
                    if token.borrow().negative_join_results.is_empty() {
                        let children = std::mem::take(&mut token.borrow_mut().children);
                        Token::delete_descendants(children);
                    }
                    let join_result = NegativeJoinResult::new(token, wme).to_cell();
                    token
                        .borrow_mut()
                        .negative_join_results
                        .push(Rc::clone(&join_result));
                    wme.borrow_mut()
                        .negative_join_results
                        .push(Rc::clone(&join_result))
                }
            }
        }
        Node::Beta(_) => unreachable!("Beta memory nodes are never right activated"),
        Node::Production(_) => unreachable!("Production nodes are never right activated"),
    }
}

fn build_or_share_beta_memory_node(parent: &RcCell<Node>) -> RcCell<Node> {
    if let Some(children) = parent.borrow().children() {
        // Look for an existing beta node to share
        for child in children {
            if let Node::Beta(ref beta) = *child.borrow() {
                println!("Shared {beta}");
                return Rc::clone(child);
            }
        }
    }

    let new = BetaMemoryNode::new(Some(Rc::clone(parent))).to_node_cell();

    parent.borrow_mut().add_child(&new);

    update_new_node_with_matches_from_above(&new);

    println!("Built {}", new.borrow());

    new
}

fn build_or_share_join_node(
    parent: &RcCell<Node>,
    alpha_memory: &RcCell<AlphaMemoryNode>,
    tests: Vec<TestAtJoinNode>,
) -> RcCell<Node> {
    if let Some(children) = parent.borrow().children() {
        // Look for an existing join node to share
        for child in children {
            if let Node::Join(node) = &*child.borrow() {
                if node.tests.as_slice() == tests
                    && *node.alpha_memory.borrow() == *alpha_memory.borrow()
                {
                    println!("Sharing {node}");
                    return Rc::clone(child);
                }
            }
        }
    }

    let new = JoinNode::new(parent, alpha_memory, tests).to_node_cell();

    // Add the newly created node to the alpha memory successors
    alpha_memory.borrow_mut().successors.push(Rc::clone(&new));

    parent.borrow_mut().add_child(&new);

    println!("Built {}", new.borrow());

    new
}

fn build_or_share_negative_node(
    parent: &RcCell<Node>,
    alpha_memory: &RcCell<AlphaMemoryNode>,
    tests: Vec<TestAtJoinNode>,
) -> RcCell<Node> {
    if let Some(children) = parent.borrow().children() {
        for child in children {
            if let Node::Negative(node) = &*child.borrow() {
                if *node.alpha_mem.borrow() == *alpha_memory.borrow() && node.tests == tests {
                    println!("Sharing {node}");
                    return Rc::clone(child);
                }
            }
        }
    }

    let new = NegativeNode::new(parent, alpha_memory, tests).to_node_cell();

    alpha_memory.borrow_mut().successors.push(Rc::clone(&new));

    update_new_node_with_matches_from_above(&new);

    println!("Built {}", new.borrow());

    new
}

fn join_test(tests: &[TestAtJoinNode], token: &RcCell<Token>, wme: &Wme) -> bool {
    println!(
        "Performing join tests on {tests:?} with WME {} {:?} and token {}",
        wme.id,
        wme.fields,
        token.borrow().id
    );

    for test in tests.iter() {
        let parent = Token::nth_parent(Rc::clone(token), test.distance_to_wme);

        // If the tokens are pointing to the dummy token they immediatelly get a pass
        if parent.borrow().id == 0 {
            println!("Join test successful");
            return true;
        }

        let parent = parent.borrow();

        // If there is no WME on the token, it represents a negative node on the token
        // which should return false since the args are not equal??
        let Some(wme2) = &parent.wme else { return false; }; //TODO figure out if this is correct

        let wme2 = wme2.borrow();
        println!("Comparing WME {:?} from token {}", wme2.fields, parent.id);

        let current_value = wme[test.arg_one];
        let previous_value = wme2[test.arg_two];

        println!(
            "Testing Current WME {:?} with Previous {:?}, {current_value} != {previous_value}",
            wme.id, wme2.id,
        );

        if current_value != previous_value {
            return false;
        }
    }

    println!("Join test successful");
    true
}

fn get_join_tests_from_condition(
    condition: &Condition,
    earlier_conds: &[Condition],
) -> Vec<TestAtJoinNode> {
    let mut result = vec![];

    println!(
        "Creating join tests from {:?} and earlier {:?}",
        condition, earlier_conds
    );

    let current_condition_num = earlier_conds.len();

    for (current_idx, var) in condition.variables() {
        let Some((distance, prev_idx)) = earlier_conds
            .iter()
            .enumerate()
            .rev()
            .find_map(|(idx, cond)| cond.variables().find_map(|(cond_idx, v)|
                if v == var {
                    Some((idx + 1, cond_idx))
                } else {
                    None
                }
            ))
        else {
            continue;
        };

        let test = TestAtJoinNode {
            arg_one: current_idx,
            distance_to_wme: current_condition_num - distance,
            arg_two: prev_idx,
        };

        result.push(test)
    }

    println!("Created join tests {:?}", result);
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::item::ConditionTest;

    #[test]
    fn it_works() {
        let mut rete = Rete::new();
        let conditions = Vec::from([Condition([
            ConditionTest::Variable(1),
            ConditionTest::Constant(2),
            ConditionTest::Variable(3),
        ])]);
        rete.add_production(Production { id: 1, conditions });
        rete.add_wme([1, 2, 3]);
    }

    #[test]
    fn join_test_to_condition() {
        use ConditionTest::*;
        let condition = Condition([Variable(2), Variable(3), Variable(4)]);
        let previous = &[
            Condition([Variable(1), Variable(1), Variable(2)]),
            Condition([Variable(5), Variable(6), Variable(3)]),
        ];
        let test_nodes = get_join_tests_from_condition(&condition, previous);

        assert_eq!(
            test_nodes[0],
            TestAtJoinNode {
                arg_one: 0,
                distance_to_wme: 1,
                arg_two: 2
            }
        );

        assert_eq!(
            test_nodes[1],
            TestAtJoinNode {
                arg_one: 1,
                distance_to_wme: 0,
                arg_two: 2
            }
        );

        let c = Condition([Variable(1), Constant(0), Variable(2)]);
        let earlier = vec![
            Condition([Variable(3), Constant(1), Variable(5)]),
            Condition([Variable(1), Constant(0), Variable(7)]),
            Condition([Variable(6), Constant(0), Variable(7)]),
        ];

        let result = get_join_tests_from_condition(&c, &earlier);

        assert_eq!(
            result[0],
            TestAtJoinNode {
                arg_one: 0,
                distance_to_wme: 1,
                arg_two: 0
            }
        );

        let c = Condition([Variable(1), Constant(0), Variable(2)]);
        let earlier = vec![
            Condition([Variable(3), Constant(1), Variable(5)]),
            Condition([Variable(2), Constant(0), Variable(7)]),
            Condition([Variable(6), Constant(0), Variable(1)]),
        ];

        let result = get_join_tests_from_condition(&c, &earlier);

        assert_eq!(
            result[0],
            TestAtJoinNode {
                arg_one: 0,
                distance_to_wme: 0,
                arg_two: 2
            }
        );
        assert_eq!(
            result[1],
            TestAtJoinNode {
                arg_one: 2,
                distance_to_wme: 1,
                arg_two: 0
            }
        );
    }

    #[test]
    fn nth_parent_works() {
        let beta = BetaMemoryNode::new(None);

        let wme = Wme::new([1, 2, 3]).to_cell();

        let node = beta.to_node_cell();

        let daddy = Token::new(&node, None, Some(&wme));

        let child_token_one = Token::new(&node, Some(&daddy), Some(&wme));

        let child_token_two = Token::new(&node, Some(&child_token_one), Some(&wme));

        child_token_one
            .borrow_mut()
            .children
            .push(Rc::clone(&child_token_two));
        daddy.borrow_mut().children.push(child_token_one);

        let parent = Token::nth_parent(child_token_two, 2);

        assert_eq!(parent.borrow().id, daddy.borrow().id);
        assert_eq!(
            parent.borrow().children.len(),
            daddy.borrow().children.len()
        );
    }
}

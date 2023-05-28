#![allow(dead_code)]
use std::{
    cell::RefCell, collections::HashMap, hash::Hash, ops::Index, rc::Rc, sync::atomic::AtomicUsize,
};

#[derive(Debug)]
pub struct Rete {
    constant_tests: HashMap<ConstantTest, RcCell<AlphaMemoryNode>>,

    dummy_top_token: RcCell<Token>,
    dummy_top_node: RcCell<Node>,

    working_memory: HashMap<Wme, Vec<RcCell<AlphaMemoryNode>>>,

    productions: HashMap<usize, Production>,
    pending_productions: Vec<usize>,
}

type RcCell<T> = Rc<RefCell<T>>;

static NODE_ID_GENERATOR: AtomicUsize = AtomicUsize::new(0);

fn id() -> usize {
    NODE_ID_GENERATOR.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}

impl Rete {
    fn new() -> Self {
        let dummy_top_node = BetaMemoryNode {
            id: id(),
            parent: None,
            children: vec![],
            items: vec![],
        };

        let dummy_top_node = Rc::new(RefCell::new(Node::Beta(dummy_top_node)));

        let dummy_top_token = Token {
            id: 0, // TODO
            parent: None,
            wme: Wme {
                id: 0,
                fields: [0, 0, 0],
                alpha_mem_items: vec![],
                tokens: vec![],
            },
            node: dummy_top_node.clone(),
            children: vec![],
        };

        let dummy_top_token = Rc::new(RefCell::new(dummy_top_token));

        dummy_top_node.borrow_mut().add_token(&dummy_top_token);

        Self {
            constant_tests: HashMap::new(),
            working_memory: HashMap::new(),
            productions: HashMap::new(),
            pending_productions: Vec::new(),
            dummy_top_node,
            dummy_top_token,
        }
    }

    fn add_wme(&mut self, wme: Wme) {
        for element in wme.permutations() {
            let Some(memory) = self.constant_tests.get(&element) else { continue };
            self.working_memory
                .entry(wme.clone())
                .and_modify(|mems| mems.push(memory.clone()))
                .or_insert_with(|| vec![memory.clone()]);
            activate_alpha_memory(memory, wme.clone())
        }
    }

    fn build_or_share_alpha_memory_node(
        &mut self,
        condition: &Condition,
    ) -> RcCell<AlphaMemoryNode> {
        let constant_test = ConstantTest::from(*condition);

        // Check whether an alpha memory like this exists
        if let Some(alpha_mem) = self.constant_tests.get(&constant_test) {
            return alpha_mem.clone();
        }

        // Alpha memory not found, create new one and insert into map
        let am = AlphaMemoryNode {
            items: vec![],
            successors: vec![],
        };

        let am = Rc::new(RefCell::new(am));

        self.constant_tests.insert(constant_test, am.clone());

        self.working_memory.iter_mut().for_each(|(wme, a_mem)| {
            if constant_test.matches(wme) {
                a_mem
                    .iter()
                    .for_each(|mem| activate_alpha_memory(mem, wme.clone()))
            }
        });

        am
    }

    fn add_production(&mut self, production: Production) {
        let conditions = &production.conditions;

        assert!(!conditions.is_empty(), "LHS of production cannot be empty");

        let mut current_node = self.dummy_top_node.clone();

        let mut earlier_conds = vec![];

        let mut tests = get_join_tests_from_condition(&conditions[0], &earlier_conds);
        let mut alpha_memory = self.build_or_share_alpha_memory_node(&conditions[0]);

        current_node = build_or_share_join_node(&current_node, &alpha_memory, &tests);

        for i in 1..conditions.len() {
            // Get the beta memory node Mi
            current_node = build_or_share_beta_memory_node(&current_node);

            earlier_conds.push(conditions[i - 1]);

            // Get the join node Ji for conition Ci
            tests = get_join_tests_from_condition(&conditions[i], &earlier_conds);
            alpha_memory = self.build_or_share_alpha_memory_node(&conditions[i]);

            current_node = build_or_share_join_node(&current_node, &alpha_memory, &tests);
        }

        let production = ProductionNode {
            id: production.id,
            parent: current_node.clone(),
            production,
        };

        let production = Rc::new(RefCell::new(Node::Production(production)));

        current_node.borrow_mut().add_child(&production);

        // Update the new node with matches from above via the current node which just became the production's parent
        update_new_node_with_matches_from_above(&production)
    }
}

fn update_new_node_with_matches_from_above(node: &RcCell<Node>) {
    let Some(parent) = node.borrow().parent() else { return; };

    println!("Updating node {}", parent.borrow().id());

    match *parent.borrow_mut() {
        Node::Beta(ref beta) => beta
            .items
            .iter()
            .for_each(|token| activate_left(node, token, token.borrow().wme.clone())),
        Node::Join(ref mut join) => {
            let children = join.children.clone();
            join.children = vec![node.clone()];
            join.alpha_memory
                .borrow_mut()
                .items
                .iter()
                .for_each(|item| activate_right(&parent, item.borrow().wme.clone()));
            join.children = children;
        }
        Node::Production(_) => panic!("Production node cannot have children"),
    };
}

pub enum ReteInput {
    AddWme(Wme),
    RemoveWme(usize),
    AddProduction(Production),
    RemoveProduction(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AlphaMemoryNode {
    items: Vec<RcCell<AlphaMemoryItem>>,
    successors: Vec<RcCell<Node>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AlphaMemoryItem {
    wme: Wme,
    alpha_memory: RcCell<AlphaMemoryNode>,
    next: Option<RcCell<Self>>,
    previous: Option<RcCell<Self>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BetaMemoryNode {
    id: usize,
    /// Has to be option because of the dummy top node with no parent
    parent: Option<RcCell<Node>>,
    children: Vec<RcCell<Node>>,
    items: Vec<RcCell<Token>>,
}

fn activate_alpha_memory(alpha_mem_node: &RcCell<AlphaMemoryNode>, mut wme: Wme) {
    // Set the item's previous pointer to the last item
    let previous = alpha_mem_node.borrow().items.iter().last().cloned();

    let item = AlphaMemoryItem {
        wme: wme.clone(),
        alpha_memory: alpha_mem_node.clone(),
        next: None,
        previous: previous.clone(),
    };

    let item = Rc::new(RefCell::new(item));

    // Append the current item to the `next` pointer of the last one
    if let Some(previous) = previous {
        previous.borrow_mut().next = Some(item.clone())
    }

    // Insert new item at the head of the node's items
    let mut alpha_mem = alpha_mem_node.borrow_mut();
    alpha_mem.items.push(item.clone());

    // Insert new item at the head of the WME alpha mem items
    wme.alpha_mem_items.push(item);

    alpha_mem
        .successors
        .iter()
        .for_each(|node| activate_right(node, wme.clone()))
}

#[inline]
fn make_token(node: RcCell<Node>, parent: Option<RcCell<Token>>, mut wme: Wme) -> RcCell<Token> {
    let token = Token {
        id: 1, // TODO
        parent: parent.clone(),
        wme: wme.clone(),
        node,
        children: vec![],
    };

    let token = Rc::new(RefCell::new(token));

    // Append token to parent's children list
    if let Some(parent) = parent {
        parent.borrow_mut().children.push(token.clone())
    }

    // Append token to the WME token list for efficient removal
    wme.tokens.push(token.clone());

    token
}

fn remove_wme(mut wme: Wme) {
    for item in wme.alpha_mem_items.iter() {
        let item = item.borrow_mut().to_owned();
        let mut alpha_mem = item.alpha_memory.borrow_mut();
        let id = wme.id;
        if let Some(i) = alpha_mem
            .items
            .iter()
            .position(|el| el.borrow().wme.id == id)
        {
            alpha_mem.items.remove(i);
        }
    }

    while let Some(token) = wme.tokens.pop() {
        delete_token_and_descendants(token)
    }
}

fn delete_token_and_descendants(token: RcCell<Token>) {
    let token = token.borrow_mut().to_owned();

    for child in token.children {
        delete_token_and_descendants(child);
    }
}

fn activate_left(node: &RcCell<Node>, parent_token: &RcCell<Token>, wme: Wme) {
    let new_token = make_token(node.clone(), Some(parent_token.clone()), wme.clone());

    match &mut *node.borrow_mut() {
        Node::Beta(ref mut beta_node) => {
            beta_node.items.push(new_token.clone());
            beta_node
                .children
                .iter()
                .for_each(|child| activate_left(child, &new_token, wme.clone()));
        }
        Node::Join(ref mut join_node) => {
            for alpha_mem_item in join_node.alpha_memory.borrow().items.iter() {
                let test = join_test(&join_node.tests, &new_token, &alpha_mem_item.borrow().wme);
                if test {
                    join_node.children.iter().for_each(|child| {
                        activate_left(child, &new_token, alpha_mem_item.borrow().wme.clone())
                    });
                }
            }
        }
        Node::Production(p_node) => {
            println!("Production node activated! {:?}", p_node.id)
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
fn activate_right(node: &RcCell<Node>, wme: Wme) {
    let node = node.borrow_mut().to_owned();
    match node {
        Node::Join(mut join_node) => {
            println!("Right activating join node {:?}", join_node.id);
            if let Node::Beta(ref parent) = *join_node.parent.borrow() {
                println!("Traversing items from parent {:?}", parent.id);
                for token in parent.items.iter() {
                    if join_test(&join_node.tests, token, &wme) {
                        join_node
                            .children
                            .iter_mut()
                            .for_each(|child| activate_left(child, token, wme.clone()))
                    }
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
            if let Node::Beta(_) = *child.borrow() {
                return child.clone();
            }
        }
    }

    let new = BetaMemoryNode {
        id: id(),
        parent: Some(parent.clone()),
        children: vec![],
        items: vec![],
    };

    let new = Rc::new(RefCell::new(Node::Beta(new)));

    update_new_node_with_matches_from_above(&new);

    new
}

fn build_or_share_join_node(
    parent: &RcCell<Node>,
    alpha_memory: &RcCell<AlphaMemoryNode>,
    tests: &[TestAtJoinNode],
) -> RcCell<Node> {
    if let Some(children) = parent.borrow().children() {
        // Look for an existing join node to share
        for child in children {
            let c = child.borrow().to_owned();
            match c {
                Node::Join(node)
                    if node.tests.as_slice() == tests
                        && *node.alpha_memory.borrow() == *alpha_memory.borrow() =>
                {
                    return child.clone()
                }
                _ => {}
            }
        }
    }

    let new = JoinNode {
        id: id(),
        parent: parent.clone(),
        children: vec![],
        alpha_memory: alpha_memory.clone(),
        tests: tests.to_vec(),
    };
    let new = Rc::new(RefCell::new(Node::Join(new)));

    // Add the newly created node to the parent's children
    alpha_memory.borrow_mut().successors.push(new.clone());

    new
}

fn get_join_tests_from_condition(
    condition: &Condition,
    earlier_conds: &[Condition],
) -> Vec<TestAtJoinNode> {
    let mut result = vec![];

    for (field_idx, var) in condition.variables() {
        let Some((i, prev_idx)) = earlier_conds
            .iter()
            .rev()
            .enumerate()
            .find_map(|(idx, cond)| cond.variables().find_map(|(cond_idx, v)|
            if v == var {
                Some((idx, cond_idx))
                } else {
                    None
                }
            ))
        else {
            continue;
        };

        let test = TestAtJoinNode {
            arg_one: field_idx,
            distance_to_wme: i,
            arg_two: prev_idx,
        };

        result.push(test)
    }

    result
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Beta(BetaMemoryNode),
    Join(JoinNode),
    Production(ProductionNode),
}

impl Node {
    fn id(&self) -> usize {
        match self {
            Node::Beta(beta) => beta.id,
            Node::Join(join) => join.id,
            Node::Production(ProductionNode { production, .. }) => production.id,
        }
    }

    fn children(&self) -> Option<&[RcCell<Node>]> {
        match self {
            Node::Beta(node) if !node.children.is_empty() => Some(&node.children),
            Node::Join(node) if !node.children.is_empty() => Some(&node.children),
            _ => None,
        }
    }

    fn parent(&self) -> Option<RcCell<Node>> {
        match self {
            Node::Beta(node) => node.parent.clone(),
            Node::Join(node) => Some(node.parent.clone()),
            Node::Production(node) => Some(node.parent.clone()),
        }
    }

    fn add_child(&mut self, node: &RcCell<Node>) {
        match self {
            Node::Beta(ref mut beta) => {
                println!(
                    "Adding child {} to Beta Node {}",
                    node.borrow().id(),
                    beta.id
                );
                beta.children.push(node.clone())
            }
            Node::Join(ref mut join) => join.children.push(node.clone()),
            Node::Production(_) => panic!("Production node cannot have children"),
        }
    }

    fn add_token(&mut self, token: &RcCell<Token>) {
        match self {
            Node::Beta(beta) => beta.items.push(token.clone()),
            _ => panic!("Node cannot contain tokens"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JoinNode {
    id: usize,
    parent: RcCell<Node>,
    alpha_memory: RcCell<AlphaMemoryNode>,
    children: Vec<RcCell<Node>>,
    tests: Vec<TestAtJoinNode>,
}

fn join_test(tests: &[TestAtJoinNode], token: &RcCell<Token>, wme: &Wme) -> bool {
    println!(
        "Performing join tests on {tests:?} with WME {:?}",
        wme.fields
    );
    for test in tests.iter() {
        let field_value = wme[test.arg_one];

        let parent = nth_parent(token.clone(), test.distance_to_wme);
        let wme2 = &parent.borrow().wme;

        let test = wme2[test.arg_two];

        if field_value != test {
            return false;
        }
    }

    true
}

/// Specifies the locations of the two fields whose values must be
/// equal in order for some variable to be bound consistently.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TestAtJoinNode {
    /// An index that ultimately indexes into a WME from the Alpha memory connected
    /// to the overlying [JoinNode] of this test node. Compared with `arg_two` to
    /// tests whether a join should succeed.
    arg_one: usize,

    /// Used to traverse a token's parents to find the WME value
    /// to compare with the first one.
    distance_to_wme: usize,

    /// An index that ultimately indexes into a WME from the parent [Token]
    /// found by the `distance_to_wme`
    arg_two: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProductionNode {
    id: usize,
    parent: RcCell<Node>,
    production: Production,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Production {
    id: usize,
    conditions: Vec<Condition>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Wme {
    id: usize,

    /// Represents [id, attribute, value]
    fields: [usize; 3],

    /// Alpha memory items which contain this WME as their element
    alpha_mem_items: Vec<RcCell<AlphaMemoryItem>>,

    /// Tokens which contain this WME as their element
    tokens: Vec<RcCell<Token>>,
}

impl Hash for Wme {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.fields.hash(state);
    }
}

impl Index<usize> for Wme {
    type Output = usize;

    fn index(&self, index: usize) -> &Self::Output {
        match index {
            0 => &self.fields[0],
            1 => &self.fields[1],
            2 => &self.fields[2],
            _ => panic!("WMEs cannot be indexed by indices higher than 2"),
        }
    }
}

impl Wme {
    fn permutations(&self) -> impl Iterator<Item = ConstantTest> {
        [
            ConstantTest([Some(self[0]), Some(self[1]), Some(self[2])]),
            ConstantTest([Some(self[0]), Some(self[1]), None]),
            ConstantTest([Some(self[0]), None, Some(self[2])]),
            ConstantTest([Some(self[0]), None, None]),
            ConstantTest([None, Some(self[1]), Some(self[2])]),
            ConstantTest([None, Some(self[1]), None]),
            ConstantTest([None, None, Some(self[2])]),
            ConstantTest([None, None, None]),
        ]
        .into_iter()
    }
}

/// When productions are added to the network, constant tests are created based on the its condition's constants.
/// If a constant exists in the condition, it will be represented by `Some(constant)` in the test.
/// A `None` in the constant test represents a wildcard.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ConstantTest([Option<usize>; 3]);

impl ConstantTest {
    fn matches(&self, wme: &Wme) -> bool {
        self.0[0].map_or(true, |s| s == wme.fields[0])
            && self.0[1].map_or(true, |s| s == wme.fields[1])
            && self.0[2].map_or(true, |s| s == wme.fields[2])
    }
}

impl From<Condition> for ConstantTest {
    fn from(value: Condition) -> Self {
        Self([value.0[0].into(), value.0[1].into(), value.0[2].into()])
    }
}

impl From<ConditionTest> for Option<usize> {
    fn from(test: ConditionTest) -> Option<usize> {
        match test {
            ConditionTest::Constant(id) => Some(id),
            ConditionTest::Variable(_) => None,
        }
    }
}

/// A test for a single symbol.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConditionTest {
    /// Test for a symbol with the given ID.
    Constant(usize),
    /// Test for any symbol, as long as it is the same symbol as other
    /// conditions with the same ID within a production.
    Variable(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Condition([ConditionTest; 3]);

impl Condition {
    /// Returns an iterator over only the variable tests, along with
    /// their indices.
    fn variables(&self) -> impl Iterator<Item = (usize, usize)> + '_ {
        self.0
            .iter()
            .enumerate()
            .filter_map(|(i, test)| match test {
                ConditionTest::Variable(id) => Some((i, *id)),
                ConditionTest::Constant(_) => None,
            })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    id: usize,
    parent: Option<RcCell<Self>>,
    wme: Wme,
    node: RcCell<Node>,
    children: Vec<RcCell<Self>>,
}

fn nth_parent(mut token: RcCell<Token>, n: usize) -> RcCell<Token> {
    for _ in 0..n {
        if let Some(ref parent) = token.clone().borrow().parent {
            token = parent.clone();
        } else {
            return token;
        };
    }
    token
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut rete = Rete::new();
        let conditions = Vec::from([Condition([
            ConditionTest::Variable(1),
            ConditionTest::Constant(2),
            ConditionTest::Variable(3),
        ])]);
        rete.add_production(Production { id: 1, conditions });
        rete.add_wme(Wme {
            id: 0,
            fields: [1, 2, 3],
            alpha_mem_items: vec![],
            tokens: vec![],
        });
        //println!("{:?}", rete);
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
        let beta = BetaMemoryNode {
            id: id(),
            parent: None,
            children: vec![],
            items: vec![],
        };

        let wme = Wme {
            id: 1,
            fields: [1, 2, 3],
            alpha_mem_items: vec![],
            tokens: vec![],
        };

        let node = Rc::new(RefCell::new(Node::Beta(beta)));

        let token = Token {
            id: 1,
            parent: None,
            wme: wme.clone(),
            node: node.clone(),
            children: vec![],
        };

        let daddy = Rc::new(RefCell::new(token));

        let child_token_one = Token {
            id: 2,
            parent: Some(daddy.clone()),
            wme: wme.clone(),
            node: node.clone(),
            children: vec![],
        };

        let first = Rc::new(RefCell::new(child_token_one));

        let child_token_two = Token {
            id: 3,
            parent: Some(first.clone()),
            wme,
            node,
            children: vec![],
        };

        let second = Rc::new(RefCell::new(child_token_two));

        first.borrow_mut().children.push(second.clone());
        daddy.borrow_mut().children.push(first);

        let parent = nth_parent(second, 2);

        assert_eq!(parent.borrow().id, daddy.borrow().id);
        assert_eq!(
            parent.borrow().children.len(),
            daddy.borrow().children.len()
        );
    }
}

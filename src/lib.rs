#![allow(dead_code)]
use std::{collections::HashMap, hash::Hash, ops::Index, ptr::NonNull};

#[derive(Debug)]
pub struct Rete {
    alpha_tests: HashMap<AlphaTest, AlphaMemoryNode>,
    alpha_network: HashMap<ConstantTestNode, AlphaMemoryNode>,

    dummy_top_token: *mut Token,
    dummy_top_node: *mut Node,

    wme_alpha_memories: HashMap<Wme, Vec<NonNull<AlphaMemoryNode>>>,

    productions: HashMap<usize, Production>,
}

impl Rete {
    unsafe fn new() -> Self {
        let dummy_top_node = BetaMemoryNode {
            parent: None,
            children: vec![],
            beta_memory: vec![],
        };

        let dummy_top_node = &mut Node::Beta(dummy_top_node) as *mut _;

        dbg!((&mut *dummy_top_node as *mut Node).is_null());

        let mut dummy_top_token = Token {
            parent: None,
            wme: Wme {
                id: 0,
                fields: [0, 0, 0],
                alpha_mem_items: vec![],
                tokens: vec![],
            },
            node: dummy_top_node,
            children: vec![],
        };

        Self {
            alpha_tests: HashMap::new(),
            alpha_network: HashMap::new(),
            wme_alpha_memories: HashMap::new(),
            productions: HashMap::new(),
            dummy_top_node,
            dummy_top_token: &mut dummy_top_token as *mut _,
        }
    }

    unsafe fn add_wme(&mut self, wme: Wme) {
        for element in wme.permutations() {
            let Some(ref mut memory) = self.alpha_tests.get_mut(&element) else { continue };
            memory.activate(wme.clone())
        }
    }

    unsafe fn build_or_share_alpha_memory_node(
        &mut self,
        condition: &Condition,
    ) -> *mut AlphaMemoryNode {
        // let constant_tests = condition.constants();
        let alpha_test = AlphaTest::from(*condition);
        if self.alpha_tests.contains_key(&alpha_test) {
            return self.alpha_tests.get_mut(&alpha_test).unwrap() as *mut _;
        }
        let am = AlphaMemoryNode {
            items: vec![],
            successors: vec![],
        };
        self.wme_alpha_memories.iter_mut().for_each(|(wme, a_mem)| {
            if alpha_test.matches(wme) {
                a_mem
                    .iter_mut()
                    .for_each(|mem| mem.as_mut().activate(wme.clone()))
            }
        });
        self.alpha_tests.insert(alpha_test, am);
        self.alpha_tests.get_mut(&alpha_test).unwrap() as *mut _
    }

    unsafe fn add_production(&mut self, production: Production) {
        let conditions = &production.conditions;
        assert!(conditions.len() > 0, "LHS of production cannot be empty");

        let mut current_node = self.dummy_top_node;

        let mut earlier_conds = vec![];

        let mut tests = get_join_tests_from_condition(&conditions[0], &earlier_conds);
        let mut alpha_memory = self.build_or_share_alpha_memory_node(&conditions[0]);

        current_node = build_or_share_join_node(current_node, alpha_memory, &tests);

        for i in 1..conditions.len() {
            // Get the beta memory node Mi
            current_node = build_or_share_beta_memory_node(current_node);

            // Get the join node Ji for conition Ci
            earlier_conds.push(conditions[i - 1]);

            tests = get_join_tests_from_condition(&conditions[i], &earlier_conds);
            alpha_memory = self.build_or_share_alpha_memory_node(&conditions[i]);

            current_node = build_or_share_join_node(current_node, alpha_memory, &tests);
        }

        let production = ProductionNode {
            tokens: vec![],
            parent: current_node,
            production,
        };

        let mut production = &mut Node::Production(production) as *mut _;

        (&mut *current_node).add_child(production);

        // Update the new node with matches from above via the current node which just became the production's parent
        match current_node.as_mut().unwrap() {
            Node::Beta(beta) => beta.beta_memory.iter_mut().for_each(|token| {
                activate_left(production.as_mut().unwrap(), token, token.wme.clone())
            }),
            Node::Join(join) => {
                join.children.push(production);
                join.alpha_memory
                    .as_mut()
                    .unwrap()
                    .items
                    .iter_mut()
                    .for_each(|item| {
                        activate_right(current_node.as_mut().unwrap(), item.wme.clone())
                    })
            }
            Node::Production(_) => panic!("Production node cannot have children"),
        };
    }
}

pub enum ReteInput {
    AddWme(Wme),
    RemoveWme(usize),
    AddProduction(Production),
    RemoveProduction(usize),
}

#[derive(Debug)]
pub struct ConstantTestNode {
    /// Indicates which component of a WME this node will test for
    component: Option<WmeComponent>,

    /// What the component's value must be equal to in order to pass
    /// the test
    condition: usize,

    /// The [Alpha memory][AlphaMemoryNode] that will get activated if the condition is met
    /// and if it exists.
    output_memory: Option<NonNull<AlphaMemoryNode>>,

    ///
    children: Vec<NonNull<ConstantTestNode>>,
}

impl ConstantTestNode {
    unsafe fn activate(&mut self, wme: Wme) {
        // If the test does not exist, it is the root node so we skip conditional checks
        if let Some(ref component) = self.component {
            let value = wme[*component];
            // If the test fails, return
            if value != self.condition {
                return;
            }
        }

        // Activate corresponding alpha memories for the node
        if let Some(ref mut memory) = self.output_memory {
            memory.as_mut().activate(wme.clone())
        }

        // Propagate the activation to the children
        self.children
            .iter_mut()
            .for_each(|child| child.as_mut().activate(wme.clone()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AlphaMemoryNode {
    items: Vec<AlphaMemoryItem>,
    successors: Vec<NonNull<Node>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AlphaMemoryItem {
    wme: Wme,
    alpha_memory: *mut AlphaMemoryNode,
    next: Option<*mut Self>,
    previous: Option<*mut Self>,
}

#[derive(Debug, Clone, Hash)]
pub struct BetaMemoryNode {
    /// Has to be option because of the dummy top node with no parent
    parent: Option<*mut Node>,
    children: Vec<*mut Node>,
    beta_memory: Vec<Token>,
}

impl AlphaMemoryNode {
    unsafe fn activate(&mut self, mut wme: Wme) {
        // Set the item's previous pointer to the last item
        let previous = self.items.iter_mut().last().map(|item| item as *mut _);

        let mut item = AlphaMemoryItem {
            wme: wme.clone(),
            alpha_memory: self as *mut _,
            next: None,
            previous,
        };

        // Append the current item to the `next` pointer of the last one
        if let Some(mut previous) = previous {
            previous.as_mut().unwrap().next = Some(&mut item as *mut _)
        }

        wme.alpha_mem_items
            .push(NonNull::new(&mut item as *mut _).expect("Pointer is null"));

        self.items.push(item);

        self.successors
            .iter_mut()
            .for_each(|node| activate_right(node.as_mut(), wme.clone()))
    }
}

unsafe fn make_token(node: *mut Node, parent: Option<NonNull<Token>>, mut wme: Wme) -> Token {
    let mut token = Token {
        parent,
        wme: wme.clone(),
        node,
        children: vec![],
    };

    // Append token to parent's children list
    if let Some(mut parent) = parent {
        parent
            .as_mut()
            .children
            .push(NonNull::new(&mut token as *mut _).expect("Impossible"))
    }

    // Append token to the WME token list for efficient removal
    wme.tokens
        .push(NonNull::new(&mut token as *mut _).expect("Impossible"));

    token
}

unsafe fn remove_wme(mut wme: Wme) {
    for item in wme.alpha_mem_items.iter_mut() {
        let alpha_mem = item.as_mut().alpha_memory.as_mut().unwrap();
        let id = wme.id;
        if let Some(i) = alpha_mem.items.iter().position(|el| el.wme.id == id) {
            alpha_mem.items.remove(i);
        }
    }
    while let Some(token) = wme.tokens.pop() {
        delete_token_and_descendants(token.as_ptr())
    }
}

unsafe fn delete_token_and_descendants(token: *mut Token) {
    let children = &mut (*token).children;
    while !children.is_empty() {
        let token = children[0];
        delete_token_and_descendants(token.as_ptr());
    }
}

unsafe fn activate_left(node: &mut Node, parent_token: &mut Token, wme: Wme) {
    let mut new_token = make_token(
        node as *mut _,
        Some(NonNull::new(parent_token as *mut _).unwrap()),
        wme.clone(),
    );

    match node {
        Node::Beta(beta_node) => {
            beta_node.beta_memory.push(new_token.clone());
            beta_node.children.iter_mut().for_each(|child| {
                activate_left(child.as_mut().unwrap(), &mut new_token, wme.clone())
            });
        }
        Node::Join(join_node) => {
            for alpha_mem_item in join_node.alpha_memory.as_mut().unwrap().items.iter() {
                if join_test(&join_node.tests, &mut new_token, &alpha_mem_item.wme) {
                    join_node.children.iter_mut().for_each(|child| {
                        activate_left(
                            child.as_mut().unwrap(),
                            &mut new_token,
                            alpha_mem_item.wme.clone(),
                        )
                    });
                }
            }
        }
        Node::Production(p_node) => {
            println!("Production node activated! {:?}", p_node)
        }
    }
}

/// A right activation of a `JoinNode` will cause it to iterate through its
/// parent's tokens and perform a [join_test] on each one and the given WME.
///
/// For every test that passes, a left activation is triggered on each of the node's
/// children.
///
/// Right activations are caused by [AlphaNode]s when [WME][Wme]s are changed or
/// when new [WME][Wme]s enter the network
unsafe fn activate_right(node: &mut Node, wme: Wme) {
    match node {
        Node::Join(join_node) => {
            if let Node::Beta(parent) = join_node.parent.as_mut().unwrap() {
                for token in parent.beta_memory.iter_mut() {
                    if join_test(&join_node.tests, token, &wme) {
                        join_node.children.iter_mut().for_each(|child| {
                            activate_left(child.as_mut().unwrap(), token, wme.clone())
                        })
                    }
                }
            }
        }
        Node::Beta(_) => unreachable!("Beta memory nodes are never right activated"),
        Node::Production(_) => unreachable!("Production nodes are never right activated"),
    }
}

unsafe fn build_or_share_beta_memory_node(parent: *mut Node) -> *mut Node {
    if let Some(children) = parent.as_ref().unwrap().children() {
        for child in children {
            match child.as_ref().unwrap() {
                Node::Beta(_) => return *child,
                _ => {}
            }
        }
    }

    let new = BetaMemoryNode {
        parent: Some(parent),
        children: vec![],
        beta_memory: vec![],
    };

    &mut Node::Beta(new) as *mut _
}

unsafe fn build_or_share_join_node(
    parent: *mut Node,
    alpha_memory: *mut AlphaMemoryNode,
    tests: &[TestAtJoinNode],
) -> *mut Node {
    if let Some(children) = parent.as_ref().unwrap().children() {
        for child in children {
            match child.as_ref().unwrap() {
                Node::Join(node)
                    if node.tests.as_slice() == tests
                        && node.alpha_memory.as_ref().unwrap()
                            == alpha_memory.as_ref().unwrap() =>
                {
                    return *child
                }
                _ => {}
            }
        }
    }

    let new = JoinNode {
        parent,
        children: vec![],
        alpha_memory,
        tests: tests.to_vec(),
    };

    &mut Node::Join(new) as *mut _
}

fn get_join_tests_from_condition(
    condition: &Condition,
    earlier_conds: &[Condition],
) -> Vec<TestAtJoinNode> {
    condition
        .variables()
        .filter_map(|(field_index, var)| {
            earlier_conds
                .iter()
                .rev()
                .enumerate()
                .find_map(|(distance, previous_condition)| {
                    previous_condition
                        .variables()
                        .find_map(|(prev_field_index, v)| {
                            if var == v {
                                Some((prev_field_index, distance))
                            } else {
                                None
                            }
                        })
                })
                .map(|(field_idx, distance_to_parent)| TestAtJoinNode {
                    arg_one: field_index,
                    distance_to_wme: distance_to_parent,
                    arg_two: field_idx,
                })
        })
        .collect()
}

#[derive(Debug, Clone)]
pub enum Node {
    Beta(BetaMemoryNode),
    Join(JoinNode),
    Production(ProductionNode),
}

impl Node {
    fn children(&self) -> Option<&[*mut Node]> {
        match self {
            Node::Beta(node) if !node.children.is_empty() => Some(&node.children),
            Node::Join(node) if !node.children.is_empty() => Some(&node.children),
            _ => None,
        }
    }

    fn add_child(&mut self, node: *mut Node) {
        match self {
            Node::Beta(ref mut beta) => beta.children.push(node),
            Node::Join(ref mut join) => join.children.push(node),
            Node::Production(_) => panic!("Production node cannot have children"),
        }
    }

    unsafe fn to_non_null(mut self) -> NonNull<Self> {
        NonNull::new(&mut self as *mut _).unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct JoinNode {
    parent: *mut Node,
    alpha_memory: *mut AlphaMemoryNode,
    children: Vec<*mut Node>,
    tests: Vec<TestAtJoinNode>,
}

fn join_test(tests: &[TestAtJoinNode], token: &Token, wme: &Wme) -> bool {
    for test in tests.iter() {
        let field_value = wme[test.arg_one];
        let wme2 = &token.nth_parent(test.distance_to_wme).wme;
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

#[derive(Debug, Clone)]
pub struct ProductionNode {
    tokens: Vec<Token>,
    parent: *mut Node,
    production: Production,
}

#[derive(Debug, Clone)]
pub struct Production {
    id: usize,
    conditions: Vec<Condition>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Wme {
    id: usize,
    fields: [usize; 3],
    alpha_mem_items: Vec<NonNull<AlphaMemoryItem>>,
    tokens: Vec<NonNull<Token>>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct AlphaTest([Option<usize>; 3]);

impl AlphaTest {
    fn matches(&self, wme: &Wme) -> bool {
        self.0[0].map_or(true, |s| s == wme.fields[0])
            && self.0[1].map_or(true, |s| s == wme.fields[1])
            && self.0[2].map_or(true, |s| s == wme.fields[2])
    }
}

impl From<Condition> for AlphaTest {
    fn from(value: Condition) -> Self {
        Self([value.0[0].into(), value.0[1].into(), value.0[2].into()])
    }
}

impl Hash for Wme {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.fields.hash(state);
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

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum WmeComponent {
    Id,
    Attribute,
    Value,
}

impl Index<WmeComponent> for Wme {
    type Output = usize;

    fn index(&self, index: WmeComponent) -> &Self::Output {
        match index {
            WmeComponent::Id => &self.fields[0],
            WmeComponent::Attribute => &self.fields[1],
            WmeComponent::Value => &self.fields[2],
        }
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
    fn permutations(&self) -> impl Iterator<Item = AlphaTest> {
        [
            AlphaTest([Some(self[0]), Some(self[1]), Some(self[2])]),
            AlphaTest([Some(self[0]), Some(self[1]), None]),
            AlphaTest([Some(self[0]), None, Some(self[2])]),
            AlphaTest([Some(self[0]), None, None]),
            AlphaTest([None, Some(self[1]), Some(self[2])]),
            AlphaTest([None, Some(self[1]), None]),
            AlphaTest([None, None, Some(self[2])]),
            AlphaTest([None, None, None]),
        ]
        .into_iter()
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
    /// Returns an iterator over only the constant tests, along with
    /// their indices.
    fn constants(&self) -> [Option<usize>; 3] {
        let mut constants = [None, None, None];
        self.0.iter().enumerate().for_each(|(i, test)| match test {
            ConditionTest::Variable(_) => {}
            ConditionTest::Constant(id) => constants[i] = Some(*id),
        });
        constants
    }

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    parent: Option<NonNull<Self>>,
    wme: Wme,
    node: *mut Node,
    children: Vec<NonNull<Self>>,
}

impl Token {
    fn nth_parent<'a>(&'a self, n: usize) -> &'a Self {
        let mut token = self;
        for _ in 0..n {
            if let Some(parent) = token.parent {
                token = unsafe { parent.as_ref() };
            } else {
                return token;
            }
        }
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        unsafe {
            let mut rete = Rete::new();
            dbg!(&rete);
            let conditions = Vec::from([Condition([
                ConditionTest::Variable(1),
                ConditionTest::Constant(2),
                ConditionTest::Variable(3),
            ])]);
            rete.add_production(Production { id: 1, conditions });
            dbg!(rete);
        }
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
    }
}

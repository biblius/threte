use std::{
    borrow::BorrowMut,
    cell::RefCell,
    collections::{HashMap, HashSet},
    ops::Index,
    rc::Rc,
};

pub struct Rete {
    alpha_tests: HashMap<Wme, ConstantTestNode>,
    alpha_network: HashMap<ConstantTestNode, AlphaMemoryNode>,
    beta_network: HashMap<Token, BetaMemoryNode>,

    productions: HashMap<usize, Production>,
}

impl Rete {
    fn add_wme(&mut self, wme: Wme) {
        for element in wme.permutations() {
            let Some(ref mut memory) = self.alpha_tests.get_mut(&element) else { continue };
            memory.activate(wme)
        }
    }
}

pub enum ReteInput {
    AddWme(Wme),
    RemoveWme(usize),
    AddProduction(Production),
    RemoveProduction(usize),
}

pub struct ConstantTestNode {
    /// Indicates which component of a WME this node will test for
    component: Option<WmeComponent>,

    /// What the component's value must be equal to in order to pass
    /// the test
    condition: WmeValue,

    /// The [Alpha memory][AlphaMemoryNode] that will get activated if the condition is met
    /// and if it exists.
    output_memory: Option<AlphaMemoryNode>,

    ///
    children: Option<Vec<ConstantTestNode>>,
}

impl ConstantTestNode {
    fn activate(&mut self, wme: Wme) {
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
            memory.activate(wme)
        }

        // Propagate the activation to the children
        if let Some(ref mut children) = self.children {
            children.iter_mut().for_each(|child| child.activate(wme))
        }
    }
}

pub struct AlphaMemoryNode {
    id: usize,
    elements: Vec<Wme>,
    successors: Vec<Node>,
}

impl AlphaMemoryNode {
    fn activate(&mut self, wme: Wme) {
        self.elements.push(wme);
        self.successors
            .iter_mut()
            .for_each(|node| node.activate_right(wme))
    }
}

pub struct BetaMemoryNode {
    id: usize,
    parent: Rc<Node>,
    children: Vec<Node>,
    beta_memory: Vec<Token>,
}

impl BetaMemoryNode {
    fn activate_left(&mut self, parent_token: Token, wme: Wme) {
        let token = Token {
            parent: Rc::new(parent_token),
            wme,
        };
        self.beta_memory.push(token.clone());
        self.children
            .iter_mut()
            .for_each(|child| child.activate_left(&token, wme));
    }
}

pub enum Node {
    Beta(BetaMemoryNode),
    Join(JoinNode),
    Production(ProductionNode),
}

impl Node {
    fn activate_left(&mut self, token: &Token, wme: Wme) {}
    fn activate_right(&mut self, wme: Wme) {}
}

pub struct JoinNode {
    id: usize,
    parent: Rc<BetaMemoryNode>,
    children: Vec<Node>,
    alpha_memory: Rc<AlphaMemoryNode>,
    tests: Vec<TestAtJoinNode>,
}

impl JoinNode {
    /// A right activation of a `JoinNode` will cause it to iterate through its
    /// parent's tokens and perform a [join_test] on each one and the given WME.
    ///
    /// For every test that passes, a left activation is triggered on each of the node's
    /// children.
    ///
    /// Right activations are caused by [AlphaNode]s when [WME][Wme]s are changed or
    /// when new [WME][Wme]s enter the network
    fn activate_right(&mut self, wme: Wme) {
        for token in self.parent.beta_memory.iter() {
            if join_test(&self.tests, token, wme) {
                self.children
                    .iter_mut()
                    .for_each(|child| child.activate_left(token, wme))
            }
        }
    }

    fn activate_left(&mut self, token: &Token) {
        for wme in self.alpha_memory.elements.iter() {
            if join_test(&self.tests, token, *wme) {
                self.children
                    .iter_mut()
                    .for_each(|child| child.activate_left(token, *wme));
            }
        }
    }
}

fn join_test(tests: &[TestAtJoinNode], token: &Token, wme: Wme) -> bool {
    for test in tests.iter() {
        let field_value = wme[test.arg_one];
        let wme2 = token.nth_parent(test.distance_to_wme).wme;
        let test = wme2[test.arg_two];
        if field_value != test {
            return false;
        }
    }
    true
}

/// Specifies the locations of the two fields whose values must be
/// equal in order for some variable to be bound consistently.
pub struct TestAtJoinNode {
    /// An index that ultimately indexes into a WME from the Alpha memory connected
    /// to the overlying [JoinNode] of this test node. Compared with `arg_two` to
    /// tests whether a join should succeed.
    arg_one: WmeComponent,

    /// Used to traverse a token's parents to find the WME value
    /// to compare with the first one.
    distance_to_wme: usize,

    /// An index that ultimately indexes into a WME from the parent [Token]
    /// found by the `distance_to_wme`
    arg_two: WmeComponent,
}

pub struct ProductionNode {
    id: usize,
    tokens: Vec<Token>,
    parent: Rc<Node>,
    production: Production,
}

pub struct Production {
    id: usize,
    conditions: Vec<Condition>,
}

enum BetaNodeActivationType {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Wme([WmeValue; 3]);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum WmeComponent {
    Id,
    Attribute,
    Value,
}

impl Index<WmeComponent> for Wme {
    type Output = WmeValue;

    fn index(&self, index: WmeComponent) -> &Self::Output {
        match index {
            WmeComponent::Id => &self.0[0],
            WmeComponent::Attribute => &self.0[1],
            WmeComponent::Value => &self.0[2],
        }
    }
}
impl Index<usize> for Wme {
    type Output = WmeValue;

    fn index(&self, index: usize) -> &Self::Output {
        match index {
            0..=2 => &self.0[index],
            _ => panic!("WMEs cannot be indexed by indices higher than 2"),
        }
    }
}

impl Wme {
    fn permutations(&self) -> impl Iterator<Item = Self> {
        use WmeValue::*;
        [
            Wme([self.0[0], self.0[1], self.0[2]]),
            Wme([self.0[0], self.0[1], Wildcard]),
            Wme([self.0[0], Wildcard, self.0[2]]),
            Wme([self.0[0], Wildcard, Wildcard]),
            Wme([Wildcard, self.0[1], self.0[2]]),
            Wme([Wildcard, self.0[1], Wildcard]),
            Wme([Wildcard, Wildcard, self.0[2]]),
            Wme([Wildcard, Wildcard, Wildcard]),
        ]
        .into_iter()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum WmeValue {
    Wildcard,
    Value(usize),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Condition((usize, usize, usize));

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    parent: Rc<Self>,
    wme: Wme,
}

impl Token {
    fn nth_parent<'a>(&'a self, n: usize) -> &'a Self {
        let mut token = self;
        for _ in 0..n {
            token = &token.parent;
        }
        token
    }
}

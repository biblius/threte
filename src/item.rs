use crate::{
    id::{item_id, token_id, wme_id},
    node::{AlphaMemoryNode, Node},
    IntoCell, RcCell,
};
use std::ops::Index;
use std::{hash::Hash, rc::Rc};

/// A WME represents a piece of state in the system.
///
/// WMEs are hashed based on their ID and index into
/// corresponding alpha memories containing them, which eliminates the need
/// to store a pointer to the memories on the actual WME.
#[derive(Debug, PartialEq)]
pub struct Wme {
    pub id: usize,

    /// Represents [id, attribute, value]
    pub fields: [usize; 3],

    /// Tokens which contain this WME as their element
    pub tokens: Vec<RcCell<Token>>,

    pub negative_join_results: Vec<RcCell<NegativeJoinResult>>,
}

impl Wme {
    pub fn new(fields: [usize; 3]) -> Self {
        Self {
            id: wme_id(),
            fields,
            tokens: vec![],
            negative_join_results: vec![],
        }
    }
    #[rustfmt::skip]
    pub fn permutations(&self) -> impl Iterator<Item = ConstantTest> {
        [
            ConstantTest([Some(self[0]), Some(self[1]), Some(self[2])]),
            ConstantTest([Some(self[0]), Some(self[1]), None]),
            ConstantTest([Some(self[0]), None,          Some(self[2])]),
            ConstantTest([Some(self[0]), None,          None]),
            ConstantTest([None,          Some(self[1]), Some(self[2])]),
            ConstantTest([None,          Some(self[1]), None]),
            ConstantTest([None,          None,          Some(self[2])]),
            ConstantTest([None,          None,          None]),
        ]
        .into_iter()
    }
}

impl IntoCell for Wme {}

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

/// A token represents a partially matched condition. Whenever beta nodes are left activated,
/// tokens are created and stored in them and are used by join nodes to perform join tests to determine
/// whether to propagate the left activation further.
#[derive(Debug)]
pub struct Token {
    pub id: usize,

    /// Pointer to the parent token. The dummy token is the only token that doesn't
    /// have a parent
    pub parent: Option<RcCell<Self>>,

    /// The WME this token represents that was partially matched. If the WME is None, the token
    /// represents a Negative Node token
    pub wme: Option<RcCell<Wme>>,

    /// The node this token belongs to
    pub node: RcCell<Node>,

    /// List of pointers to this token's children
    pub children: Vec<RcCell<Self>>,

    // TODO separate the next three fields into a token variant
    // as they are only used by negative and ncc nodes
    /// List of all successful join results performed by negative nodes. This field is used
    /// solely by negative nodes to determine whether to propagate activations.
    pub negative_join_results: Vec<RcCell<NegativeJoinResult>>,

    /// Solely used by NCC nodes
    pub ncc_results: Vec<RcCell<Token>>,

    /// An owner token that stores this one in case this one is an NCC partner node token.
    pub owner: Option<RcCell<Token>>,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.wme == other.wme && self.children == other.children
    }
}

impl Token {
    /// Mutably borrows the parent_token and wme to append them to their
    /// respective lists
    pub fn new(
        node: &RcCell<Node>,
        parent_token: Option<&RcCell<Self>>,
        wme: Option<&RcCell<Wme>>,
    ) -> RcCell<Self> {
        let token = Self {
            id: token_id(),
            parent: parent_token.cloned(),
            wme: wme.cloned(),
            node: node.clone(),
            children: vec![],
            negative_join_results: vec![],
            ncc_results: vec![],
            owner: None,
        };

        println!(
            "Creating token {} and appending to token {}",
            token,
            parent_token.as_ref().map_or(0, |t| t.borrow().id)
        );

        let token = token.to_cell();

        if let Some(parent) = parent_token {
            parent.borrow_mut().children.push(Rc::clone(&token))
        }

        // Append token to the WME token list for efficient removal
        if let Some(wme) = wme {
            wme.borrow_mut().tokens.push(Rc::clone(&token));
        }

        token
    }

    pub fn nth_parent(mut token: RcCell<Self>, n: usize) -> RcCell<Self> {
        for _ in 0..n {
            if let Some(ref parent) = Rc::clone(&token).borrow().parent {
                token = Rc::clone(parent)
            } else {
                println!("Found {n}th parent token: {}", token.borrow().id);
                return token;
            };
        }
        println!("Found {n}th parent token: {}", token.borrow().id);
        token
    }

    /// Clean up the token and any of its descendants from the WME linked to it, its parent,
    /// and the node it is stored in.
    pub fn delete_self_and_descendants(token: RcCell<Self>) {
        println!(
            "Deleting token {}, refs: {}",
            token.borrow(),
            Rc::strong_count(&token)
        );

        let mut tok = token.borrow_mut();

        let id = tok.id;
        let parent = tok.parent.take();
        let wme = tok.wme.take();
        let node = Rc::clone(&tok.node);
        let children = std::mem::take(&mut tok.children);
        let negative_joins = std::mem::take(&mut tok.negative_join_results);

        drop(tok);

        println!("Deleting descendants of token {id}");
        Self::delete_descendants(children);

        node.borrow_mut().remove_token(id);

        if let Node::Negative(_) = &*node.borrow() {
            for result in negative_joins {
                result
                    .borrow()
                    .wme
                    .borrow_mut()
                    .negative_join_results
                    .retain(|res| res.borrow().id != result.borrow().id)
            }
        }

        if let Some(ref wme) = wme {
            println!("Removing token {id} from wme {}", wme.borrow().id);
            wme.borrow_mut().tokens.retain(|tok| tok.borrow().id != id)
        }

        if let Some(parent) = parent {
            println!("Removing token {id} from parent {}", parent.borrow().id);
            parent
                .borrow_mut()
                .children
                .retain(|child| child.borrow().id != id)
        }
    }

    #[inline]
    pub fn delete_descendants(mut children: Vec<RcCell<Token>>) {
        while let Some(child) = children.pop() {
            Self::delete_self_and_descendants(child);
        }
    }
}

impl IntoCell for Token {}

/// Enables for quick splicing of WMEs. Since a single WME could be located
/// in many alpha memories, this serves as an indirection that points to it.
/// These are stored by alpha memories whenever a WME enters the network.
#[derive(Debug, PartialEq)]
pub struct AlphaMemoryItem {
    pub id: usize,
    pub wme: RcCell<Wme>,
    pub alpha_memory: RcCell<AlphaMemoryNode>,
}

impl AlphaMemoryItem {
    pub fn new(wme: &RcCell<Wme>, alpha_memory: &RcCell<AlphaMemoryNode>) -> Self {
        Self {
            id: item_id(),
            wme: Rc::clone(wme),
            alpha_memory: Rc::clone(alpha_memory),
        }
    }
}

impl IntoCell for AlphaMemoryItem {}

/// Specifies the locations of the two fields whose values must be
/// equal in order for some variable to be bound consistently.
///
/// These are stored by beta nodes and are used to perform join tests.
#[derive(Debug, Eq, PartialEq)]
pub struct TestAtJoinNode {
    /// An index that ultimately indexes into a WME from the Alpha memory connected
    /// to the overlying [JoinNode] of this test node. Compared with `arg_two` to
    /// tests whether a join should succeed.
    pub arg_one: usize,

    /// Used to traverse a token's parents to find the WME value
    /// to compare with the first one.
    pub distance_to_wme: usize,

    /// An index that ultimately indexes into a WME from the parent [Token]
    /// found by the `distance_to_wme`
    pub arg_two: usize,
}

/// When productions are added to the network, constant tests are created based on the condition's constants that
/// index into the appropriate alpha memories for the condition.
/// If a constant exists in the condition, it will be represented by `Some(constant)` in the test.
/// A `None` in the constant test represents a wildcard.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ConstantTest([Option<usize>; 3]);

impl ConstantTest {
    pub fn matches(&self, wme: &Wme) -> bool {
        self.0[0].map_or(true, |s| s == wme.fields[0])
            && self.0[1].map_or(true, |s| s == wme.fields[1])
            && self.0[2].map_or(true, |s| s == wme.fields[2])
    }
}

impl From<Condition> for ConstantTest {
    fn from(condition: Condition) -> Self {
        Self([
            condition.tests[0].into(),
            condition.tests[1].into(),
            condition.tests[2].into(),
        ])
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

#[derive(Debug)]
pub struct Production {
    pub id: usize,
    pub conditions: Vec<Condition>,
}

/// A test for a single symbol.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConditionTest {
    /// Test for a symbol with the given ID.
    Constant(usize),
    /// Test for any symbol, as long as it is the same symbol as other
    /// conditions with the same ID within a production.
    Variable(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Condition {
    pub _type: ConditionType,
    pub tests: [ConditionTest; 3],
}

impl Condition {
    pub const fn new(_type: ConditionType, tests: [ConditionTest; 3]) -> Self {
        Self { _type, tests }
    }
    /// Returns an iterator over only the variable tests, along with
    /// their indices.
    pub fn variables(&self) -> impl Iterator<Item = (usize, usize)> + '_ {
        self.tests
            .iter()
            .enumerate()
            .filter_map(|(i, test)| match test {
                ConditionTest::Variable(id) => Some((i, *id)),
                ConditionTest::Constant(_) => None,
            })
    }

    pub fn _type(&self) -> ConditionType {
        self._type
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConditionType {
    Positive,
    Negative,
    NegativeConjunction,
}

/// A negative join results represents a successful join test performed by a negative node.
/// Whenever negative nodes are activated they will perform join tests, just like join nodes,
/// and will store the result in their corresponding tokens and will propagate the activation further
/// only if the join tests do NOT pass for the WME in question.
#[derive(Debug, PartialEq)]
pub struct NegativeJoinResult {
    pub id: usize,
    /// The token in whose local memory this result resides in
    pub owner: RcCell<Token>,

    /// The WME that matches the owner
    pub wme: RcCell<Wme>,
}

impl NegativeJoinResult {
    pub fn new(owner: &RcCell<Token>, wme: &RcCell<Wme>) -> Self {
        Self {
            id: item_id(),
            owner: Rc::clone(owner),
            wme: Rc::clone(wme),
        }
    }
}

impl IntoCell for NegativeJoinResult {}

use crate::{
    node::{AlphaMemoryNode, Node},
    token_id, RcCell,
};
use std::ops::Index;
use std::{cell::RefCell, hash::Hash, rc::Rc};

/// A WME represents a piece of state in the system.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Wme {
    pub id: usize,

    /// Represents [id, attribute, value]
    pub fields: [usize; 3],

    /// Alpha memory items which contain this WME as their element
    pub alpha_mem_items: Vec<RcCell<AlphaMemoryItem>>,

    /// Tokens which contain this WME as their element
    pub tokens: Vec<RcCell<Token>>,
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
    pub fn new(id: usize, fields: [usize; 3]) -> Self {
        Self {
            id,
            fields,
            alpha_mem_items: vec![],
            tokens: vec![],
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AlphaMemoryItem {
    pub id: usize,
    pub wme: Wme,
    pub alpha_memory: RcCell<AlphaMemoryNode>,
    pub next: Option<RcCell<Self>>,
    pub previous: Option<RcCell<Self>>,
}

/// Specifies the locations of the two fields whose values must be
/// equal in order for some variable to be bound consistently.
#[derive(Debug, Clone, Eq, PartialEq)]
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

/// A token represents a partially matched condition. Whenever beta nodes are left activated,
/// tokens are created and stored in them and are used by join nodes to perform join tests to determine
/// whether to propagate the left activation further.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub id: usize,

    /// Pointer to the parent token. The dummy token is the only token that doesn't
    /// have a parent
    pub parent: Option<RcCell<Self>>,

    /// The WME this token represents that was partially matched
    pub wme: Wme,

    /// The Beta Node this token belongs to
    pub node: RcCell<Node>,

    /// List of pointers to this token's children
    pub children: Vec<RcCell<Self>>,
}
impl Token {
    pub fn new(
        node: RcCell<Node>,
        parent_token: Option<RcCell<Self>>,
        wme: &mut Wme,
    ) -> RcCell<Self> {
        let token = Self {
            id: token_id(),
            parent: parent_token.clone(),
            wme: wme.clone(),
            node,
            children: vec![],
        };

        println!(
            "Creating token {} and appending to token {}",
            token,
            parent_token.as_ref().unwrap().borrow().id,
        );

        let token = Rc::new(RefCell::new(token));

        if let Some(parent) = parent_token {
            parent.borrow_mut().children.push(token.clone())
        }

        // Append token to the WME token list for efficient removal
        wme.tokens.push(token.clone());

        token
    }

    pub fn nth_parent(mut token: RcCell<Self>, n: usize) -> RcCell<Self> {
        for _ in 0..n {
            if let Some(ref parent) = token.clone().borrow().parent {
                token = parent.clone();
            } else {
                println!("Found {n}th parent token: {}", token.borrow().id);
                return token;
            };
        }
        println!("Found {n}th parent token: {}", token.borrow().id);
        token
    }

    pub fn delete_self_and_descendants(token: RcCell<Self>) {
        let token = token.borrow_mut();

        for child in token.children.iter() {
            Self::delete_self_and_descendants(child.clone());
        }
    }
}

/// When productions are added to the network, constant tests are created based on the its condition's constants.
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
pub struct Condition(pub [ConditionTest; 3]);

impl Condition {
    /// Returns an iterator over only the variable tests, along with
    /// their indices.
    pub fn variables(&self) -> impl Iterator<Item = (usize, usize)> + '_ {
        self.0
            .iter()
            .enumerate()
            .filter_map(|(i, test)| match test {
                ConditionTest::Variable(id) => Some((i, *id)),
                ConditionTest::Constant(_) => None,
            })
    }
}

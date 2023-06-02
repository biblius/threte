use crate::{
    id::{item_id, token_id, wme_id},
    node::{AlphaMemoryNode, Node},
    IntoCell, RcCell,
};
use std::{cell::RefCell, ops::Index, rc::Weak};
use std::{hash::Hash, rc::Rc};

/// A WME represents a piece of state in the system.
///
/// Clone is derived solely for the initial setup of a WME, it should not be cloned
/// once in the system. WMEs are hashed based on their ID and fields and index into
/// corresponding alpha memories containing them.
#[derive(Debug, Clone, PartialEq)]
pub struct Wme {
    pub id: usize,

    /// Represents [id, attribute, value]
    pub fields: [usize; 3],

    /// Tokens which contain this WME as their element
    pub tokens: Vec<RcCell<Token>>,
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

impl Wme {
    pub fn new(fields: [usize; 3]) -> Self {
        Self {
            id: wme_id(),
            fields,
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

/// Enables for quick splicing of WMEs. Since a single WME could be located
/// in many alpha memories, this serves as an indirection.
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

/// A token represents a partially matched condition. Whenever beta nodes are left activated,
/// tokens are created and stored in them and are used by join nodes to perform join tests to determine
/// whether to propagate the left activation further.
#[derive(Debug)]
pub struct Token {
    pub id: usize,

    /// Pointer to the parent token. The dummy token is the only token that doesn't
    /// have a parent
    pub parent: Option<Weak<RefCell<Self>>>,

    /// The WME this token represents that was partially matched
    pub wme: RcCell<Wme>,

    /// The Beta Node this token belongs to
    pub node: RcCell<Node>,

    /// List of pointers to this token's children
    pub children: Vec<RcCell<Self>>,
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
        parent_token: Option<Weak<RefCell<Self>>>,
        wme: &RcCell<Wme>,
    ) -> RcCell<Self> {
        let token = Self {
            id: token_id(),
            parent: parent_token.clone(),
            wme: Rc::clone(wme),
            node: node.clone(),
            children: vec![],
        };

        println!(
            "Creating token {} and appending to token {}",
            token,
            parent_token
                .as_ref()
                .map_or(0, |t| t.upgrade().unwrap().borrow().id)
        );

        let token = token.to_cell();

        if let Some(parent) = parent_token {
            parent
                .upgrade()
                .unwrap()
                .borrow_mut()
                .children
                .push(Rc::clone(&token))
        }

        // Append token to the WME token list for efficient removal
        wme.borrow_mut().tokens.push(Rc::clone(&token));

        token
    }

    pub fn nth_parent(mut token: RcCell<Self>, n: usize) -> RcCell<Self> {
        for _ in 0..n {
            if let Some(ref parent) = Rc::clone(&token).borrow().parent {
                token = parent.upgrade().unwrap();
            } else {
                println!("Found {n}th parent token: {}", token.borrow().id);
                return token;
            };
        }
        println!("Found {n}th parent token: {}", token.borrow().id);
        token
    }

    ///
    pub fn delete_self_and_descendants(token: RcCell<Self>) {
        println!(
            "Deleting token {}, refs: {}",
            token.borrow(),
            Rc::strong_count(&token)
        );
        let mut tok = token.borrow_mut();

        let id = tok.id;
        let parent = tok.parent.take();
        let children = std::mem::take(&mut tok.children);
        let node = Rc::clone(&tok.node);

        drop(tok);

        node.borrow_mut().remove_token(id);

        if let Some(parent) = parent {
            parent
                .upgrade()
                .unwrap()
                .borrow_mut()
                .children
                .retain(|child| child.borrow().id != id)
        }

        for child in children {
            Self::delete_self_and_descendants(child);
        }
    }
}

impl IntoCell for Token {}

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

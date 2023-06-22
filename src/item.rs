use crate::{
    activate_left,
    id::{item_id, token_id, wme_id},
    node::{AlphaMemoryNode, Node, ReteNode},
    IntoCell, RcCell,
};
use std::ops::Index;
use std::{hash::Hash, rc::Rc};

pub const DUMMY_TOKEN_ID: usize = usize::MIN;

pub type ReteToken = RcCell<Token>;

/// A WME represents a piece of state in the system.
///
/// WMEs are hashed based on their ID and index into
/// corresponding alpha memories containing them, which eliminates the need
/// to store a pointer to the memories on the actual WME.
#[derive(Debug)]
pub struct Wme {
    pub id: usize,

    /// Represents [id, attribute, value]
    pub fields: [usize; 3],

    /// Tokens which contain this WME as their element
    pub tokens: Vec<ReteToken>,

    pub negative_join_results: Vec<RcCell<NegativeJoinResult>>,
}

impl PartialEq for Wme {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.fields == other.fields
    }
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

/// A token represents a partially matched condition. Whenever memory nodes are left activated,
/// tokens are created and stored in them and are used by join nodes to perform join tests to determine
/// whether to propagate the left activation further.
#[derive(Debug)]
pub enum Token {
    Dummy {
        id: usize,
        node: ReteNode,
        children: Vec<ReteToken>,
    },
    Beta {
        base: TokenBase,
    },
    Negative {
        base: TokenBase,

        /// List of all successful join results performed by negative nodes. This field is used
        /// solely by negative nodes to determine whether to propagate activations.
        join_results: Vec<RcCell<NegativeJoinResult>>,
    },
    NCC {
        base: TokenBase,

        /// The local memory of this token
        ncc_results: Vec<ReteToken>,

        /// An owner token that stores this one in its local memory in case this one is an NCC partner node token.
        owner: Option<ReteToken>,
    },
}

#[derive(Debug)]
pub struct TokenBase {
    pub id: usize,

    /// The node this token belongs to
    pub node: ReteNode,

    /// Pointer to the parent token. The dummy token is the only token that doesn't
    /// have a parent.
    pub parent: ReteToken,

    /// List of pointers to this token's children
    pub children: Vec<ReteToken>,

    /// The WME this token represents that was partially matched. If the WME is None, the token
    /// represents a Negative Node token
    pub wme: Option<RcCell<Wme>>,
}

impl TokenBase {
    pub fn new(node: &ReteNode, parent: &ReteToken, wme: Option<&RcCell<Wme>>) -> Self {
        Self {
            id: token_id(),
            node: Rc::clone(node),
            parent: Rc::clone(parent),
            children: vec![],
            wme: wme.cloned(),
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::Beta { base }, Token::Beta { base: _base }) => {
                base.id == _base.id && base.wme == _base.wme && base.children == _base.children
            }
            (Token::Negative { base, .. }, Token::Negative { base: _base, .. }) => {
                base.id == _base.id && base.wme == _base.wme && base.children == _base.children
            }
            (Token::NCC { base, .. }, Token::NCC { base: _base, .. }) => {
                base.id == _base.id && base.wme == _base.wme && base.children == _base.children
            }
            _ => false,
        }
    }
}

impl Token {
    /// Mutably borrows the parent_token and wme to append them to their
    /// respective lists
    pub fn new_beta(
        node: &ReteNode,
        parent: &RcCell<Self>,
        wme: Option<&RcCell<Wme>>,
    ) -> RcCell<Self> {
        let token = Self::Beta {
            base: TokenBase::new(node, parent, wme),
        };

        println!(
            "Creating token {} and appending to token {}",
            token,
            parent.as_ref().borrow().id()
        );

        let token = token.to_cell();

        parent.borrow_mut().add_child(&token);

        if let Some(wme) = wme {
            wme.borrow_mut().tokens.push(Rc::clone(&token));
        }

        token
    }

    pub fn new_negative(
        node: &ReteNode,
        parent: &RcCell<Self>,
        wme: Option<&RcCell<Wme>>,
    ) -> RcCell<Self> {
        let token = Self::Negative {
            base: TokenBase::new(node, parent, wme),
            join_results: vec![],
        };

        println!(
            "Creating token {} and appending to token {}",
            token,
            parent.as_ref().borrow().id()
        );

        let token = token.to_cell();

        parent.borrow_mut().add_child(&token);

        if let Some(wme) = wme {
            wme.borrow_mut().tokens.push(Rc::clone(&token));
        }

        token
    }

    pub fn new_ncc(
        node: &ReteNode,
        parent: &RcCell<Self>,
        wme: Option<&RcCell<Wme>>,
    ) -> RcCell<Self> {
        let token = Self::NCC {
            base: TokenBase::new(node, parent, wme),
            ncc_results: vec![],
            owner: None,
        };

        println!(
            "Creating token {} and appending to token {}",
            token,
            parent.as_ref().borrow().id()
        );

        let token = token.to_cell();

        parent.borrow_mut().add_child(&token);

        if let Some(wme) = wme {
            wme.borrow_mut().tokens.push(Rc::clone(&token));
        }

        token
    }

    /// Used when instantiating the network
    pub fn new_dummy(dummy_top_node: &ReteNode) -> RcCell<Self> {
        Self::Dummy {
            id: DUMMY_TOKEN_ID,
            node: Rc::clone(dummy_top_node),
            children: vec![],
        }
        .to_cell()
    }

    #[inline]
    pub fn base_mut(&mut self) -> &mut TokenBase {
        match self {
            Token::Beta { base } => base,
            Token::Negative { base, .. } => base,
            Token::NCC { base, .. } => base,
            Token::Dummy { .. } => panic!("wtf"),
        }
    }

    #[inline]
    pub fn id(&self) -> usize {
        match self {
            Token::Dummy { id, .. } => *id,
            Token::Beta { base } => base.id,
            Token::Negative { base, .. } => base.id,
            Token::NCC { base, .. } => base.id,
        }
    }

    #[inline]
    pub fn wme(&self) -> Option<&RcCell<Wme>> {
        match self {
            Token::Beta { base } => base.wme.as_ref(),
            Token::Negative { base, .. } => base.wme.as_ref(),
            Token::NCC { base, .. } => base.wme.as_ref(),
            Token::Dummy { .. } => None,
        }
    }

    #[inline]
    pub fn add_child(&mut self, child: &ReteToken) {
        match self {
            Token::Dummy { children, .. } => children.push(Rc::clone(child)),
            Token::Beta { base } => base.children.push(Rc::clone(child)),
            Token::Negative { base, .. } => base.children.push(Rc::clone(child)),
            Token::NCC { base, .. } => base.children.push(Rc::clone(child)),
        }
    }

    #[inline]
    pub fn remove_child(&mut self, id: usize) {
        match self {
            Token::Dummy { children, .. } => children.retain(|c| c.borrow().id() != id),
            Token::Beta { base } => base.children.retain(|c| c.borrow().id() != id),
            Token::Negative { base, .. } => base.children.retain(|c| c.borrow().id() != id),
            Token::NCC { base, .. } => base.children.retain(|c| c.borrow().id() != id),
        }
    }

    #[inline]
    pub fn children(&self) -> &[ReteToken] {
        match self {
            Token::Dummy { children, .. } => children,
            Token::Beta { base } => &base.children,
            Token::Negative { base, .. } => &base.children,
            Token::NCC { base, .. } => &base.children,
        }
    }

    #[inline]
    pub fn children_mut(&mut self) -> &mut Vec<ReteToken> {
        match self {
            Token::Dummy {
                ref mut children, ..
            } => children,
            Token::Beta { base } => &mut base.children,
            Token::Negative { base, .. } => &mut base.children,
            Token::NCC { base, .. } => &mut base.children,
        }
    }

    #[inline]
    pub fn parent(&self) -> Option<&ReteToken> {
        match self {
            Token::Dummy { .. } => None,
            Token::Beta { base } => Some(&base.parent),
            Token::Negative { base, .. } => Some(&base.parent),
            Token::NCC { base, .. } => Some(&base.parent),
        }
    }

    #[inline]
    pub fn node(&self) -> &ReteNode {
        match self {
            Token::Dummy { node, .. } => node,
            Token::Beta { base } => &base.node,
            Token::Negative { base, .. } => &base.node,
            Token::NCC { base, .. } => &base.node,
        }
    }

    #[inline]
    pub fn set_owner(&mut self, new_owner: &ReteToken) {
        let Token::NCC { ref mut owner, .. } = self else {panic!("Token cannot contain owner")};
        *owner = Some(Rc::clone(new_owner))
    }

    #[inline]
    /// Returns true if the token is a `Negative` token and its `join_results` are not empty
    pub fn contains_join_results(&self) -> bool {
        let Token::Negative {  join_results, .. } = self else { return false };
        !join_results.is_empty()
    }

    pub fn add_join_result(&mut self, result: &RcCell<NegativeJoinResult>) {
        let Token::Negative {  join_results, .. } = self else { panic!("Token cannot contain negative result") };
        join_results.push(Rc::clone(result))
    }

    #[inline]
    pub fn remove_join_result(&mut self, id: usize) -> bool {
        let Token::Negative {  join_results, .. } = self else { panic!("Token cannot contain negative result") };
        join_results.retain(|res| res.borrow().id != id);
        join_results.is_empty()
    }

    #[inline]
    /// Returns true if the token is an `NCC` token and its `ncc_results` are not empty
    pub fn contains_ncc_results(&self) -> bool {
        let Token::NCC {  ncc_results, .. } = self else { return false };
        !ncc_results.is_empty()
    }

    #[inline]
    pub fn add_ncc_result(&mut self, token: &ReteToken) {
        let Token::NCC {  ncc_results, .. } = self else { panic!("Token cannot contain NCC result") };
        ncc_results.push(Rc::clone(token))
    }

    /// Returns `true` only if the token was an `NCC` token and
    /// it contains no more ncc results after the removal.
    #[inline]
    pub fn remove_ncc_result(&mut self, id: usize) -> bool {
        let Token::NCC {  ncc_results, .. } = self else { return false; };
        ncc_results.retain(|res| res.borrow().id() != id);
        ncc_results.is_empty()
    }

    #[inline]
    /// Returns the `n`th ancestor of a token by following the parent pointer from the provided
    /// one
    pub fn nth_parent(mut token: RcCell<Self>, n: usize) -> RcCell<Self> {
        for _ in 0..n {
            if let Some(parent) = Rc::clone(&token).borrow().parent() {
                token = Rc::clone(parent)
            } else {
                println!("Found {n}th parent token: {}", token.borrow().id());
                return token;
            };
        }
        println!("Found {n}th parent token: {}", token.borrow().id());
        token
    }

    /// Clean up the token and any of its descendants from the WME linked to it, its parent,
    /// and the node it is stored in. Also remove
    pub fn delete_self_and_descendants(token: RcCell<Self>) {
        println!(
            "Deleting token {}, refs: {}",
            token.borrow(),
            Rc::strong_count(&token)
        );

        let DestructuredToken {
            id,
            node,
            parent,
            children,
            wme,
            join_results,
            ncc_results,
            owner,
        } = {
            let mut tok = token.borrow_mut();
            Token::destructure(&mut tok)
        };

        println!("Deleting descendants of token {id}");

        Self::delete_descendants(children);

        // Remove from corresponding node, wme and parent token
        if !matches!(&*node.borrow(), Node::NccPartner(_)) {
            node.borrow_mut().remove_token(id);
        }

        if let Some(wme) = wme {
            println!("Removing token {id} from wme {}", wme.borrow().id);
            wme.borrow_mut()
                .tokens
                .retain(|tok| tok.borrow().id() != id)
        }

        if let Some(parent) = parent {
            println!("Removing token {id} from parent {}", parent.borrow().id());
            parent.borrow_mut().remove_child(id);
        }

        // Right unlink the token's node if it became empty, the check is done in the method
        node.borrow().right_unlink();
        if let Node::Negative(ref mut neg) = *node.borrow_mut() {
            neg.right_linked = false;
        }

        // Remove all negative join results from corresponding WME
        for result in join_results {
            let result = result.borrow();
            let wme = &mut *result.wme.borrow_mut();
            println!(
                "Removing result from WME {}'s negative join results",
                wme.id
            );
            wme.negative_join_results
                .retain(|res| res.borrow().id != result.id)
        }

        // Remove all NCC results from corresponding WME
        for result in ncc_results {
            let result = &mut result.borrow_mut();
            if let Some(wme) = result.base_mut().wme.take() {
                println!(
                    "Removing token {} from WME {}'s NCC results",
                    result.id(),
                    wme.borrow().id
                );
                wme.borrow_mut().tokens.retain(|t| t.borrow().id() != id)
            }

            result.base_mut().parent.borrow_mut().remove_child(id);
        }

        if let Node::NccPartner(node) = &*node.borrow() {
            if let Some(owner) = owner {
                if owner.borrow_mut().remove_ncc_result(id) {
                    for child in node.ncc_node.borrow().children() {
                        activate_left(child, &owner, None)
                    }
                }
            }
        }

        let _ = node; // rust pls
    }

    /// Run `delete_self_and_descendants` on the provided children vec
    #[inline]
    pub fn delete_descendants(children: Vec<ReteToken>) {
        for child in children.into_iter() {
            Self::delete_self_and_descendants(child);
        }
    }

    /// Destructure the token to avoid the mutable reference when deleting
    #[inline]
    fn destructure(&mut self) -> DestructuredToken {
        match self {
            Token::Beta {
                base:
                    TokenBase {
                        id,
                        node,
                        parent,
                        children,
                        wme,
                    },
            } => DestructuredToken {
                id: *id,
                node: Rc::clone(node),
                parent: Some(Rc::clone(parent)),
                children: std::mem::take(children),
                wme: wme.take(),
                join_results: vec![],
                ncc_results: vec![],
                owner: None,
            },
            Token::Negative {
                base:
                    TokenBase {
                        id,
                        node,
                        parent,
                        children,
                        wme,
                    },
                join_results,
            } => DestructuredToken {
                id: *id,
                node: Rc::clone(node),
                parent: Some(Rc::clone(parent)),
                children: std::mem::take(children),
                wme: wme.take(),
                join_results: std::mem::take(join_results),
                ncc_results: vec![],
                owner: None,
            },
            Token::NCC {
                base:
                    TokenBase {
                        id,
                        node,
                        parent,
                        children,
                        wme,
                    },
                ncc_results,
                owner,
            } => DestructuredToken {
                id: *id,
                node: Rc::clone(node),
                parent: Some(Rc::clone(parent)),
                children: std::mem::take(children),
                wme: wme.take(),
                join_results: vec![],
                ncc_results: std::mem::take(ncc_results),
                owner: owner.take(),
            },
            Token::Dummy { id, node, children } => DestructuredToken {
                id: *id,
                node: Rc::clone(node),
                parent: None,
                children: std::mem::take(children),
                wme: None,
                join_results: vec![],
                ncc_results: vec![],
                owner: None,
            },
        }
    }
}

impl IntoCell for Token {}

/// Used to destructure the token so as to avoid keeping a mutable reference to the original
/// when deleting
struct DestructuredToken {
    id: usize,
    node: ReteNode,
    parent: Option<ReteToken>,
    children: Vec<ReteToken>,
    wme: Option<RcCell<Wme>>,
    join_results: Vec<RcCell<NegativeJoinResult>>,
    ncc_results: Vec<ReteToken>,
    owner: Option<ReteToken>,
}

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
pub struct JoinTest {
    /// An index that ultimately indexes into a WME from the Alpha memory connected
    /// to the node holding this test. Compared with `arg_two` to
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ConstantTest([Option<usize>; 3]);

impl ConstantTest {
    pub fn matches(&self, wme: &Wme) -> bool {
        self.0[0].map_or(true, |s| s == wme.fields[0])
            && self.0[1].map_or(true, |s| s == wme.fields[1])
            && self.0[2].map_or(true, |s| s == wme.fields[2])
    }
}

pub fn conditions_to_constant_tests(acc: &mut Vec<ConstantTest>, conditions: &[Condition]) {
    for condition in conditions {
        match condition {
            Condition::Positive { .. } | Condition::Negative { .. } => {
                acc.push(ConstantTest::from(condition))
            }
            Condition::NegativeConjunction { subconditions } => {
                conditions_to_constant_tests(acc, subconditions);
            }
        }
    }
}

impl From<Condition> for ConstantTest {
    fn from(condition: Condition) -> Self {
        match condition {
            Condition::Positive { test } | Condition::Negative { test } => {
                Self([test[0].into(), test[1].into(), test[2].into()])
            }
            Condition::NegativeConjunction { .. } => {
                panic!("Cannot convert condition to constant test")
            }
        }
    }
}
impl From<&Condition> for ConstantTest {
    fn from(condition: &Condition) -> Self {
        match condition {
            Condition::Positive { test } | Condition::Negative { test } => {
                Self([test[0].into(), test[1].into(), test[2].into()])
            }
            Condition::NegativeConjunction { .. } => {
                panic!("Cannot convert condition to constant test")
            }
        }
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Condition {
    Positive { test: [ConditionTest; 3] },
    Negative { test: [ConditionTest; 3] },
    NegativeConjunction { subconditions: Vec<Self> },
}

impl Condition {
    pub const fn new_positive(test: [ConditionTest; 3]) -> Self {
        Self::Positive { test }
    }

    pub const fn new_negative(test: [ConditionTest; 3]) -> Self {
        Self::Negative { test }
    }

    pub fn new_ncc(subconditions: Vec<Self>) -> Self {
        Self::NegativeConjunction { subconditions }
    }

    /// Returns an iterator over only the variable test, along with
    /// their indices.
    pub fn variables(&self) -> impl Iterator<Item = (usize, usize)> + '_ {
        match self {
            Condition::Positive { test } | Condition::Negative { test } => {
                test.iter().enumerate().filter_map(|(i, test)| match test {
                    ConditionTest::Variable(id) => Some((i, *id)),
                    ConditionTest::Constant(_) => None,
                })
            }
            Condition::NegativeConjunction { .. } => {
                panic!("NCC Condition does not have variables")
            }
        }
    }
}

/// A negative join results represents a successful join test performed by a negative node.
/// Whenever negative nodes are activated they will perform join tests,
/// store the result in their corresponding tokens and will propagate the activation further
/// only if the join tests do NOT pass for the WME in question.
#[derive(Debug, PartialEq)]
pub struct NegativeJoinResult {
    pub id: usize,

    /// The token in whose local memory this result resides in
    pub owner: ReteToken,

    /// The WME held by the owner
    pub wme: RcCell<Wme>,
}

impl NegativeJoinResult {
    pub fn new(owner: &ReteToken, wme: &RcCell<Wme>) -> Self {
        Self {
            id: item_id(),
            owner: Rc::clone(owner),
            wme: Rc::clone(wme),
        }
    }
}

impl IntoCell for NegativeJoinResult {}

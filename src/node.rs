use crate::{
    id::{alpha_node_id, beta_node_id},
    item::{AlphaMemoryItem, Production, TestAtJoinNode, Token},
    IntoCell, IntoNodeCell, RcCell,
};
use std::rc::Rc;

pub type ReteNode = RcCell<Node>;

pub const DUMMY_NODE_ID: usize = usize::MIN;

#[derive(Debug)]
pub enum Node {
    Beta(BetaMemoryNode),
    Join(JoinNode),
    Production(ProductionNode),
    Negative(NegativeNode),
    Ncc(NccNode),
    NccPartner(NccPartnerNode),
}

impl Node {
    #[inline]
    pub fn id(&self) -> usize {
        match self {
            Node::Beta(beta) => beta.id,
            Node::Join(join) => join.id,
            Node::Negative(negative) => negative.id,
            Node::Production(ProductionNode { production, .. }) => production.id,
            Node::Ncc(ncc) => ncc.id,
            Node::NccPartner(ncc_partner) => ncc_partner.id,
        }
    }

    #[inline]
    pub fn _type(&self) -> &str {
        match self {
            Node::Beta(_) => "beta",
            Node::Join(_) => "join",
            Node::Negative(_) => "negative",
            Node::Production(_) => "prod",
            Node::Ncc(_) => "ncc",
            Node::NccPartner(_) => "ncc-partner",
        }
    }

    #[inline]
    pub fn children(&self) -> Option<&[ReteNode]> {
        match self {
            Node::Beta(node) if !node.children.is_empty() => Some(&node.children),
            Node::Join(node) if !node.children.is_empty() => Some(&node.children),
            Node::Negative(node) if !node.children.is_empty() => Some(&node.children),
            Node::Ncc(node) if !node.children.is_empty() => Some(&node.children),
            _ => None,
        }
    }

    #[inline]
    pub fn tokens(&self) -> Option<&[RcCell<Token>]> {
        match self {
            Node::Beta(node) if !node.items.is_empty() => Some(&node.items),
            Node::Negative(node) if !node.items.is_empty() => Some(&node.items),
            Node::Ncc(node) if !node.items.is_empty() => Some(&node.items),
            _ => None,
        }
    }

    #[inline]
    pub fn parent(&self) -> Option<ReteNode> {
        match self {
            Node::Beta(node) => node.parent.clone(),
            Node::Join(node) => Some(Rc::clone(&node.parent)),
            Node::Negative(node) => Some(Rc::clone(&node.parent)),
            Node::Ncc(node) => Some(Rc::clone(&node.parent)),
            Node::Production(node) => Some(Rc::clone(&node.parent)),
            Node::NccPartner(node) => Some(Rc::clone(&node.parent)),
        }
    }

    #[inline]
    pub fn add_child(&mut self, node: &ReteNode) {
        match self {
            Node::Beta(ref mut beta) => {
                println!(
                    "add_child - Adding child {} - {} to Beta Node {}",
                    node.borrow()._type(),
                    node.borrow().id(),
                    beta.id
                );
                beta.children.push(Rc::clone(node))
            }
            Node::Join(ref mut join) => {
                println!(
                    "add_child - Adding child {} - {} to Join Node {}",
                    node.borrow()._type(),
                    node.borrow().id(),
                    join.id
                );
                join.children.push(Rc::clone(node))
            }
            Node::Negative(ref mut negative) => {
                println!(
                    "add_child - Adding child {} - {} to Join Node {}",
                    node.borrow()._type(),
                    node.borrow().id(),
                    negative.id
                );
                negative.children.push(Rc::clone(node))
            }
            Node::Ncc(ref mut ncc) => {
                println!(
                    "add_child - Adding child {} - {} to Join Node {}",
                    node.borrow()._type(),
                    node.borrow().id(),
                    ncc.id
                );
                ncc.children.push(Rc::clone(node))
            }
            Node::Production(_) => unreachable!("Production Node cannot have children"),
            Node::NccPartner(_) => unreachable!("NCC Partner Node cannot have children"),
        }
    }

    #[inline]
    pub fn remove_child(&mut self, id: usize) {
        match self {
            Node::Beta(beta) => {
                println!("remove_child - Removing {} from Beta Node {}", id, beta.id);
                beta.children.retain(|child| child.borrow().id() != id)
            }
            Node::Join(join) => {
                println!("remove_child - Removing {} from Join Node {}", id, join.id);
                join.children.retain(|child| child.borrow().id() != id)
            }
            Node::Negative(negative) => {
                println!(
                    "remove_child - Removing {} from Negative Node {}",
                    id, negative.id
                );
                negative.children.retain(|child| child.borrow().id() != id)
            }
            Node::Ncc(ncc) => {
                println!("remove_child - Removing {} from NCC Node {}", id, ncc.id);
                ncc.children.retain(|child| child.borrow().id() != id)
            }
            Node::Production(_) => unreachable!("Production Node cannot have children"),
            Node::NccPartner(_) => unreachable!("NCC Partner Node cannot have children"),
        }
    }

    #[inline]
    pub fn add_token(&mut self, token: &RcCell<Token>) {
        match self {
            Node::Beta(beta) => beta.items.push(Rc::clone(token)),
            Node::Negative(negative) => negative.items.push(Rc::clone(token)),
            Node::Ncc(ncc) => ncc.items.push(Rc::clone(token)),
            _ => unreachable!("Node cannot contain tokens"),
        }
    }

    #[inline]
    pub fn remove_token(&mut self, id: usize) {
        println!("Node {} removing token {}", self.id(), id);
        match self {
            Node::Beta(beta) => beta.items.retain(|tok| tok.borrow().id() != id),
            Node::Negative(negative) => negative.items.retain(|tok| tok.borrow().id() != id),
            Node::Ncc(ncc) => ncc.items.retain(|tok| tok.borrow().id() != id),
            _ => unreachable!("Node cannot contain tokens"),
        }
    }
}

/// An AlphaMemoryNode contains items through which it keeps the state of WMEs that
/// passed constant tests.
#[derive(Debug, Default)]
pub struct AlphaMemoryNode {
    pub id: usize,
    pub items: Vec<RcCell<AlphaMemoryItem>>,
    pub successors: Vec<ReteNode>,
}

impl AlphaMemoryNode {
    pub fn new() -> Self {
        let am = Self {
            id: alpha_node_id(),
            items: vec![],
            successors: vec![],
        };
        println!("Created Alpha Memory: {am}");
        am
    }
}

impl PartialEq for AlphaMemoryNode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl IntoCell for AlphaMemoryNode {}

#[derive(Debug)]
pub struct BetaMemoryNode {
    pub id: usize,
    pub parent: Option<ReteNode>,
    pub children: Vec<ReteNode>,
    pub items: Vec<RcCell<Token>>,
}

impl BetaMemoryNode {
    pub fn new(parent: Option<ReteNode>) -> Self {
        Self {
            id: beta_node_id(),
            parent,
            children: vec![],
            items: vec![],
        }
    }

    pub fn dummy() -> ReteNode {
        println!("Initiating dummy Beta Node");
        Self {
            id: DUMMY_NODE_ID,
            parent: None,
            children: vec![],
            items: vec![],
        }
        .to_node_cell()
    }
}

#[derive(Debug)]
pub struct JoinNode {
    pub id: usize,
    pub parent: ReteNode,
    pub alpha_memory: RcCell<AlphaMemoryNode>,
    pub children: Vec<ReteNode>,
    pub tests: Vec<TestAtJoinNode>,
}

impl JoinNode {
    pub fn new(
        parent: &ReteNode,
        alpha_memory: &RcCell<AlphaMemoryNode>,
        tests: Vec<TestAtJoinNode>,
    ) -> Self {
        Self {
            id: beta_node_id(),
            parent: parent.clone(),
            alpha_memory: alpha_memory.clone(),
            children: vec![],
            tests,
        }
    }
}

#[derive(Debug)]
pub struct ProductionNode {
    pub id: usize,
    pub parent: ReteNode,
    pub production: Production,
}

impl ProductionNode {
    pub fn new(prod: Production, parent: &ReteNode) -> Self {
        let node = Self {
            id: prod.id,
            parent: Rc::clone(parent),
            production: prod,
        };

        println!("Created production node {node}");

        node
    }
}

/// Negative nodes test for the absence of a certain WME in the working memory and act
/// like a combination of a Beta and Join node.
/// They keep a local memory of tokens like Beta nodes and only propagate the
/// activation when those tokens do NOT pass the join tests.
#[derive(Debug)]
pub struct NegativeNode {
    pub id: usize,
    pub items: Vec<RcCell<Token>>,
    pub alpha_mem: RcCell<AlphaMemoryNode>,
    pub tests: Vec<TestAtJoinNode>,
    pub parent: ReteNode,
    pub children: Vec<ReteNode>,
}

impl NegativeNode {
    pub fn new(
        parent: &ReteNode,
        alpha_mem: &RcCell<AlphaMemoryNode>,
        tests: Vec<TestAtJoinNode>,
    ) -> Self {
        Self {
            id: beta_node_id(),
            items: vec![],
            alpha_mem: Rc::clone(alpha_mem),
            tests,
            parent: Rc::clone(parent),
            children: vec![],
        }
    }
}

#[derive(Debug)]
pub struct NccNode {
    pub id: usize,
    pub parent: ReteNode,
    pub children: Vec<ReteNode>,
    pub items: Vec<RcCell<Token>>,

    /// This field is an option solely because we cannot construct an ncc node and its
    /// partner simultaneously so we need some kind of way to instantiate one without the other.
    ///
    /// This will never be None while the node is alive in the network.
    pub partner: Option<ReteNode>,
}

impl NccNode {
    pub fn new(parent: &ReteNode) -> Self {
        Self {
            id: beta_node_id(),
            parent: Rc::clone(parent),
            children: vec![],
            items: vec![],
            partner: None,
        }
    }
}

#[derive(Debug)]
pub struct NccPartnerNode {
    pub id: usize,
    pub parent: ReteNode,
    pub number_of_conjucts: usize,
    pub ncc_node: ReteNode,
    pub new_results: Vec<RcCell<Token>>,
}

impl NccPartnerNode {
    pub fn new(ncc_node: &ReteNode, parent: &ReteNode, number_of_conjucts: usize) -> Self {
        Self {
            id: beta_node_id(),
            parent: Rc::clone(parent),
            number_of_conjucts,
            ncc_node: Rc::clone(ncc_node),
            new_results: vec![],
        }
    }
}

impl PartialEq for NegativeNode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
            && self.items == other.items
            && self.alpha_mem == other.alpha_mem
            && self.tests == other.tests
    }
}

impl IntoNodeCell for BetaMemoryNode {
    fn to_node_cell(self) -> ReteNode {
        std::rc::Rc::new(std::cell::RefCell::new(Node::Beta(self)))
    }
}

impl IntoNodeCell for JoinNode {
    fn to_node_cell(self) -> ReteNode {
        std::rc::Rc::new(std::cell::RefCell::new(Node::Join(self)))
    }
}

impl IntoNodeCell for ProductionNode {
    fn to_node_cell(self) -> ReteNode {
        std::rc::Rc::new(std::cell::RefCell::new(Node::Production(self)))
    }
}

impl IntoNodeCell for NegativeNode {
    fn to_node_cell(self) -> ReteNode {
        std::rc::Rc::new(std::cell::RefCell::new(Node::Negative(self)))
    }
}

impl IntoNodeCell for NccNode {
    fn to_node_cell(self) -> ReteNode {
        std::rc::Rc::new(std::cell::RefCell::new(Node::Ncc(self)))
    }
}

impl IntoNodeCell for NccPartnerNode {
    fn to_node_cell(self) -> ReteNode {
        std::rc::Rc::new(std::cell::RefCell::new(Node::NccPartner(self)))
    }
}

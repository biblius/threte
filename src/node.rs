use crate::{
    id::{alpha_node_id, beta_node_id},
    item::{AlphaMemoryItem, Production, TestAtJoinNode, Token},
    IntoCell, IntoNodeCell, RcCell,
};
use std::rc::Rc;

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
    pub fn children(&self) -> Option<&[RcCell<Node>]> {
        match self {
            Node::Beta(node) if !node.children.is_empty() => Some(&node.children),
            Node::Join(node) if !node.children.is_empty() => Some(&node.children),
            Node::Negative(node) if !node.children.is_empty() => Some(&node.children),
            Node::Ncc(node) if !node.children.is_empty() => Some(&node.children),
            _ => None,
        }
    }

    #[inline]
    pub fn parent(&self) -> Option<RcCell<Node>> {
        match self {
            Node::Beta(node) => node.parent.clone(),
            Node::Join(node) => Some(Rc::clone(&node.parent)),
            Node::Negative(node) => Some(Rc::clone(&node.parent)),
            Node::Ncc(node) => Some(Rc::clone(&node.parent)),
            Node::Production(node) => Some(Rc::clone(&node.parent)),
            Node::NccPartner(_) => None,
        }
    }

    #[inline]
    pub fn add_child(&mut self, node: &RcCell<Node>) {
        match self {
            Node::Beta(ref mut beta) => {
                println!(
                    "Adding child {} to Beta Node {}",
                    node.borrow().id(),
                    beta.id
                );
                beta.children.push(Rc::clone(node))
            }
            Node::Join(ref mut join) => join.children.push(Rc::clone(node)),
            Node::Negative(ref mut negative) => negative.children.push(Rc::clone(node)),
            Node::Ncc(ref mut ncc) => ncc.children.push(Rc::clone(node)),
            Node::Production(_) => unreachable!("Production Node cannot have children"),
            Node::NccPartner(_) => unreachable!("NCC Partner Node cannot have children"),
        }
    }

    #[inline]
    pub fn remove_child(&mut self, id: usize) {
        match self {
            Node::Beta(beta) => beta.children.retain(|child| child.borrow().id() != id),
            Node::Join(join) => join.children.retain(|child| child.borrow().id() != id),
            Node::Negative(negative) => negative.children.retain(|child| child.borrow().id() != id),
            Node::Ncc(ncc) => ncc.children.retain(|child| child.borrow().id() != id),
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
            Node::Beta(beta) => beta.items.retain(|tok| tok.borrow().id != id),
            Node::Negative(negative) => negative.items.retain(|tok| tok.borrow().id != id),
            Node::Ncc(ncc) => ncc.items.retain(|tok| tok.borrow().id != id),
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
    pub successors: Vec<RcCell<Node>>,
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
    pub parent: Option<RcCell<Node>>,
    pub children: Vec<RcCell<Node>>,
    pub items: Vec<RcCell<Token>>,
}

impl BetaMemoryNode {
    pub fn new(parent: Option<RcCell<Node>>) -> Self {
        Self {
            id: beta_node_id(),
            parent,
            children: vec![],
            items: vec![],
        }
    }
}

#[derive(Debug)]
pub struct JoinNode {
    pub id: usize,
    pub parent: RcCell<Node>,
    pub alpha_memory: RcCell<AlphaMemoryNode>,
    pub children: Vec<RcCell<Node>>,
    pub tests: Vec<TestAtJoinNode>,
}

impl JoinNode {
    pub fn new(
        parent: &RcCell<Node>,
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
    pub parent: RcCell<Node>,
    pub production: Production,
}

impl ProductionNode {
    pub fn new(prod: Production, parent: &RcCell<Node>) -> Self {
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
    pub parent: RcCell<Node>,
    pub children: Vec<RcCell<Node>>,
}

impl NegativeNode {
    pub fn new(
        parent: &RcCell<Node>,
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
    pub parent: RcCell<Node>,
    pub children: Vec<RcCell<Node>>,
    pub items: Vec<RcCell<Token>>,
    pub partner: RcCell<Node>,
}

#[derive(Debug)]
pub struct NccPartnerNode {
    pub id: usize,
    pub number_of_conjucts: usize,
    pub ncc_node: RcCell<Node>,
    pub new_results: Vec<RcCell<Token>>,
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
    fn to_node_cell(self) -> RcCell<Node> {
        std::rc::Rc::new(std::cell::RefCell::new(Node::Beta(self)))
    }
}
impl IntoNodeCell for JoinNode {
    fn to_node_cell(self) -> RcCell<Node> {
        std::rc::Rc::new(std::cell::RefCell::new(Node::Join(self)))
    }
}
impl IntoNodeCell for ProductionNode {
    fn to_node_cell(self) -> RcCell<Node> {
        std::rc::Rc::new(std::cell::RefCell::new(Node::Production(self)))
    }
}

impl IntoNodeCell for NegativeNode {
    fn to_node_cell(self) -> RcCell<Node> {
        std::rc::Rc::new(std::cell::RefCell::new(Node::Negative(self)))
    }
}

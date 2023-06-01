use crate::{
    id::{alpha_node_id, beta_node_id},
    item::{AlphaMemoryItem, Condition, TestAtJoinNode, Token},
    IntoCell, IntoNodeCell, RcCell,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Node {
    Beta(BetaMemoryNode),
    Join(JoinNode),
    Production(ProductionNode),
}

impl Node {
    pub fn id(&self) -> usize {
        match self {
            Node::Beta(beta) => beta.id,
            Node::Join(join) => join.id,
            Node::Production(ProductionNode { production, .. }) => production.id,
        }
    }

    pub fn _type(&self) -> &str {
        match self {
            Node::Beta(_) => "beta",
            Node::Join(_) => "join",
            Node::Production(_) => "prod",
        }
    }

    pub fn children(&self) -> Option<&[RcCell<Node>]> {
        match self {
            Node::Beta(node) if !node.children.is_empty() => Some(&node.children),
            Node::Join(node) if !node.children.is_empty() => Some(&node.children),
            _ => None,
        }
    }

    pub fn parent(&self) -> Option<RcCell<Node>> {
        match self {
            Node::Beta(node) => node.parent.clone(),
            Node::Join(node) => Some(node.parent.clone()),
            Node::Production(node) => Some(node.parent.clone()),
        }
    }

    pub fn add_child(&mut self, node: &RcCell<Node>) {
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

    pub fn add_token(&mut self, token: &RcCell<Token>) {
        match self {
            Node::Beta(beta) => beta.items.push(token.clone()),
            _ => panic!("Node cannot contain tokens"),
        }
    }
}

/// An AlphaMemoryNode contains items through which it keeps the state of WMEs that
/// passed constant tests.
#[derive(Debug, PartialEq, Eq)]
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
        println!("Created Alpha Memory: {}", am);
        am
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub struct ProductionNode {
    pub id: usize,
    pub parent: RcCell<Node>,
    pub production: Production,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Production {
    pub id: usize,
    pub conditions: Vec<Condition>,
}

impl IntoCell for AlphaMemoryNode {}

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

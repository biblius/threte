use crate::{
    id::{alpha_node_id, beta_node_id},
    item::{AlphaMemoryItem, JoinTest, Production, Token},
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
    pub fn children(&self) -> &[ReteNode] {
        match self {
            Node::Beta(node) => &node.children,
            Node::Join(node) => &node.children,
            Node::Negative(node) => &node.children,
            Node::Ncc(node) => &node.children,
            _ => &[],
        }
    }

    #[inline]
    pub fn all_children(&self) -> &[ReteNode] {
        match self {
            Node::Beta(node) => &node.all_children,
            Node::Join(node) => &node.children,
            Node::Negative(node) => &node.children,
            Node::Ncc(node) => &node.children,
            _ => &[],
        }
    }

    #[inline]
    pub fn tokens(&self) -> &[RcCell<Token>] {
        match self {
            Node::Beta(node) => &node.items,
            Node::Negative(node) => &node.items,
            Node::Ncc(node) => &node.items,
            _ => &[],
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

    /// If the node is a join or negative node, this method will remove it from
    /// its corresponding alpha memory if the node does not contain any more items.
    ///
    /// Right unlinking makes sure no unnecessary work is performed when traversing the Rete
    /// due to activations.
    #[inline]
    pub fn right_unlink(&self) {
        match self {
            Node::Beta(node) => {
                if node.items.is_empty() {
                    for child in node.children.iter() {
                        if let Node::Join(ref join) = *child.borrow() {
                            join.alpha_mem
                                .borrow_mut()
                                .successors
                                .retain(|suc| suc.borrow().id() != join.id);
                        }
                        if let Node::Negative(ref negative) = *child.borrow() {
                            negative
                                .alpha_mem
                                .borrow_mut()
                                .successors
                                .retain(|suc| suc.borrow().id() != negative.id);
                        }
                        match &mut *child.borrow_mut() {
                            Node::Join(node) => {
                                println!("💥 Right unlinking {}", node.id);
                                node.right_linked = false;
                            }
                            Node::Negative(node) => {
                                println!("💥 Right unlinking {}", node.id);
                                node.right_linked = false;
                            }
                            _ => {}
                        }
                    }
                }
            }
            Node::Negative(node) => {
                if node.items.is_empty() {
                    node.alpha_mem
                        .borrow_mut()
                        .successors
                        .retain(|suc| suc.borrow().id() != node.id);
                }
            }
            _ => {}
        }
    }

    /// Right relinking must take into account the ordering of the successors in the alpha memory, as we
    /// always want to activate descendants before ancestors.
    #[inline]
    pub fn relink_to_alpha_mem(node: &ReteNode) {
        match &*node.borrow() {
            Node::Join(join) => {
                println!(
                    "🔗 Relinking join {} to alpha memory {}",
                    join.id,
                    join.alpha_mem.borrow().id
                );
                let mut ancestor = join.nearest_ancestor.clone();
                while let Some(anc) = ancestor.clone() {
                    let anc = anc.borrow();
                    if !anc.is_right_linked() {
                        ancestor = anc.nearest_ancestor();
                        continue;
                    }
                    break;
                }
                println!(
                    "Found nearest ancestor with same alpha mem: {:?}",
                    ancestor.as_ref().map(|a| a.borrow().id())
                );
                // We have to maintain the ordering of the ancestor, i.e. we always
                // need to splice in the current node before the ancestor, since we
                // know the current node is its descendant and must be activated before it
                if let Some(anc) = ancestor {
                    let index = join
                        .alpha_mem
                        .borrow()
                        .successors
                        .iter()
                        .position(|suc| suc.borrow().id() == anc.borrow().id())
                        .unwrap();
                    join.alpha_mem
                        .borrow_mut()
                        .successors
                        .insert(index + 1, Rc::clone(node))
                } else {
                    join.alpha_mem.borrow_mut().successors.push(Rc::clone(node))
                }
            }
            Node::Negative(negative) => {
                let mut ancestor = negative.nearest_ancestor.clone();
                while let Some(anc) = ancestor.clone() {
                    let anc = &*anc.borrow();
                    if !anc.is_right_linked() {
                        ancestor = anc.nearest_ancestor();
                    }
                }
                if let Some(anc) = ancestor {
                    let index = negative
                        .alpha_mem
                        .borrow()
                        .successors
                        .iter()
                        .position(|suc| suc.borrow().id() == anc.borrow().id())
                        .unwrap();
                    negative
                        .alpha_mem
                        .borrow_mut()
                        .successors
                        .insert(index + 1, Rc::clone(node))
                } else {
                    negative
                        .alpha_mem
                        .borrow_mut()
                        .successors
                        .push(Rc::clone(node))
                }
            }
            _ => {}
        };

        match &mut *node.borrow_mut() {
            Node::Join(node) => node.right_linked = true,
            Node::Negative(node) => node.right_linked = true,
            _ => {}
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

    #[inline]
    pub fn is_left_linked(&self) -> bool {
        match self {
            Node::Join(node) => node.left_linked,
            Node::Negative(node) => node.right_linked,
            _ => true,
        }
    }

    #[inline]
    pub fn is_right_linked(&self) -> bool {
        match self {
            Node::Join(node) => node.right_linked,
            Node::Negative(node) => node.right_linked,
            _ => true,
        }
    }

    #[inline]
    pub fn nearest_ancestor(&self) -> Option<ReteNode> {
        match self {
            Node::Join(node) => node.nearest_ancestor.clone(),
            Node::Negative(node) => node.nearest_ancestor.clone(),
            _ => None,
        }
    }

    #[inline]
    pub fn is_dummy(&self) -> bool {
        if let Node::Beta(beta) = self {
            beta.id == DUMMY_NODE_ID
        } else {
            false
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
    pub all_children: Vec<ReteNode>,
}

impl BetaMemoryNode {
    pub fn new(parent: Option<ReteNode>) -> Self {
        Self {
            id: beta_node_id(),
            parent,
            children: vec![],
            items: vec![],
            all_children: vec![],
        }
    }

    pub fn dummy() -> ReteNode {
        println!("Initiating dummy Beta Node");
        Self {
            id: DUMMY_NODE_ID,
            parent: None,
            children: vec![],
            items: vec![],
            all_children: vec![],
        }
        .to_node_cell()
    }
}

#[derive(Debug)]
pub struct JoinNode {
    pub id: usize,
    pub parent: ReteNode,
    pub alpha_mem: RcCell<AlphaMemoryNode>,
    pub children: Vec<ReteNode>,
    pub tests: Vec<JoinTest>,
    /// Indicates the nearest ancestor node with the same
    /// alpha memory as this one. Used for relinking.
    pub nearest_ancestor: Option<ReteNode>,
    pub left_linked: bool,
    pub right_linked: bool,
}

impl JoinNode {
    pub fn new(
        parent: &ReteNode,
        alpha_mem: &RcCell<AlphaMemoryNode>,
        tests: Vec<JoinTest>,
    ) -> Self {
        Self {
            id: beta_node_id(),
            parent: parent.clone(),
            alpha_mem: alpha_mem.clone(),
            children: vec![],
            tests,
            nearest_ancestor: None,
            left_linked: true,
            right_linked: true,
        }
    }
}

/// Negative nodes test for the absence of a certain WME in the working memory and act
/// like a combination of a Beta and Join node.
/// They keep a local memory of tokens like Beta nodes and only propagate the
/// activation when those tokens do NOT pass the join tests.
#[derive(Debug)]
pub struct NegativeNode {
    pub id: usize,
    pub parent: ReteNode,
    pub children: Vec<ReteNode>,
    pub items: Vec<RcCell<Token>>,
    pub alpha_mem: RcCell<AlphaMemoryNode>,
    pub tests: Vec<JoinTest>,
    pub nearest_ancestor: Option<ReteNode>,
    pub right_linked: bool,
}

impl NegativeNode {
    pub fn new(
        parent: &ReteNode,
        alpha_mem: &RcCell<AlphaMemoryNode>,
        tests: Vec<JoinTest>,
    ) -> Self {
        Self {
            id: beta_node_id(),
            items: vec![],
            alpha_mem: Rc::clone(alpha_mem),
            tests,
            parent: Rc::clone(parent),
            children: vec![],
            nearest_ancestor: None,
            right_linked: true,
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

/// The subnetwork formed by an NCC node contains no matches, it means the NCC node is eligible
/// for activation.
#[derive(Debug)]
pub struct NccNode {
    pub id: usize,
    pub parent: ReteNode,
    pub children: Vec<ReteNode>,
    pub items: Vec<RcCell<Token>>,

    /// This field is an option solely because we cannot construct an ncc node and its
    /// partner simultaneously so we need some kind of way to instantiate one without the other. (in safe code)
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

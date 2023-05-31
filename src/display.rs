use crate::{
    item::{AlphaMemoryItem, Token},
    node::{AlphaMemoryNode, BetaMemoryNode, JoinNode, Node, ProductionNode},
};
use std::fmt::Write;

impl std::fmt::Display for AlphaMemoryNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let items = self.items.iter().fold(String::new(), |mut acc, el| {
            write!(acc, "{}", el.borrow()).unwrap();
            acc
        });
        write!(
            f,
            "Alpha {{ id: {}, successors: {:?}, items: {} }}",
            self.id,
            self.successors
                .iter()
                .map(|n| (*n.borrow()).id())
                .collect::<Vec<_>>(),
            items
        )
    }
}

impl std::fmt::Display for AlphaMemoryItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "AlphaItem {{ wme: {:?}, alpha_mem: {}, next: {:?}, previous: {:?} }}",
            self.wme.fields,
            self.alpha_memory.borrow().id,
            self.next.as_ref().map(|next| next.borrow().id),
            self.previous.as_ref().map(|prev| prev.borrow().id)
        )
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Beta(beta) => write!(f, "{}", beta),
            Node::Join(join) => {
                write!(f, "{}", join)
            }
            Node::Production(prod) => {
                write!(f, "{}", prod)
            }
        }
    }
}

impl std::fmt::Display for ProductionNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Prod {{ id: {} }}", self.id,)
    }
}

impl std::fmt::Display for BetaMemoryNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Beta {{ id: {}, items: {:?}, parent: {:?}, children: {:?} }}",
            self.id,
            self.items
                .iter()
                .map(|item| item.borrow().id)
                .collect::<Vec<_>>(),
            self.parent.as_ref().map(|p| p.borrow().id()),
            self.children
                .iter()
                .map(|node| node.borrow().id())
                .collect::<Vec<_>>()
        )
    }
}

impl std::fmt::Display for JoinNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Join {{ id: {}, parent: {:?}, children: {:?} }}",
            self.id,
            self.parent.borrow().id(),
            self.children
                .iter()
                .map(|node| node.borrow().id())
                .collect::<Vec<_>>()
        )
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token {{ id: {}, parent: {:?}, wme: {:?} }}",
            self.id,
            self.parent.as_ref().map(|t| t.borrow().id),
            self.wme.fields,
        )
    }
}

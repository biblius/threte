use crate::{
    item::{AlphaMemoryItem, ConstantTest, Token},
    node::{AlphaMemoryNode, BetaMemoryNode, JoinNode, Node, ProductionNode},
    RcCell, Rete,
};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter, Result, Write},
};

impl Display for AlphaMemoryNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let items = self.items.iter().fold(String::new(), |mut acc, el| {
            write!(
                acc,
                "\n\t{}",
                el.try_borrow()
                    .map_or_else(|_| "borrowed".to_string(), |el| el.id.to_string())
            )
            .unwrap();
            acc
        });
        write!(
            f,
            "Alpha {{ id: {}, successors: {:?}, items: {} }}",
            self.id,
            self.successors
                .iter()
                .map(|n| n
                    .try_borrow()
                    .map_or_else(|_| "borrowed".to_string(), |el| el.id().to_string()))
                .collect::<Vec<_>>(),
            items
        )
    }
}

impl Display for AlphaMemoryItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "AlphaItem {{ id: {}, wme: {:?}, alpha_mem: {},}}",
            self.id,
            self.wme.borrow().fields,
            self.alpha_memory.borrow().id,
        )
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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

impl Display for ProductionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "Prod {{ id: {} }}", self.id,)
    }
}

impl Display for BetaMemoryNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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

impl Display for JoinNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "Token {{ id: {}, parent: {:?}, wme: {:?}, node: {} }}",
            self.id,
            self.parent.as_ref().map(|t| t.borrow().id),
            self.wme.borrow().fields,
            self.node
                .try_borrow()
                .map_or("borrowed".to_string(), |n| n.id().to_string())
        )
    }
}

impl Rete {
    pub fn print_to_file(&self) -> std::result::Result<(), std::io::Error> {
        let mut buf = match std::fs::read_to_string("printed") {
            Ok(buf) => buf,
            Err(_) => {
                std::fs::write("state.txt", "").unwrap();
                std::fs::read_to_string("state.txt").unwrap()
            }
        };
        writeln!(buf, "TOKENS\n").unwrap();
        write_tokens(&mut buf, self.dummy_top_token.clone());
        writeln!(buf, "\nBETA NETWORK\n").unwrap();
        write_beta_network(&mut buf, self.dummy_top_node.clone());
        writeln!(buf, "\nALPHA NETWORK\n").unwrap();
        write_alpha_network(&mut buf, &self.constant_tests);
        std::fs::write("state.txt", buf)?;
        Ok(())
    }
}

fn write_alpha_network(buf: &mut String, alpha: &HashMap<ConstantTest, RcCell<AlphaMemoryNode>>) {
    let mut items = alpha.iter().collect::<Vec<_>>();
    items.sort_by(|a, b| a.1.borrow().id.cmp(&b.1.borrow().id));
    for (test, alpha) in items {
        writeln!(buf, "{:?} :\n{}\n", test, alpha.borrow()).unwrap();
    }
}

fn write_beta_network(buf: &mut String, node: RcCell<Node>) {
    let node = node.borrow();
    writeln!(
        buf,
        "{}{}",
        " ".repeat(node.parent().as_ref().map_or(0, |p| p.borrow().id() * 2)),
        node
    )
    .unwrap();

    let Some(children) = node.children() else { return; };

    for child in children {
        write_beta_network(buf, child.clone())
    }
}

fn write_tokens(buf: &mut String, token: RcCell<Token>) {
    let tok = token.borrow();
    writeln!(
        buf,
        "{}{}",
        " ".repeat(tok.parent.as_ref().map_or(0, |p| p.borrow().id * 2)),
        tok
    )
    .unwrap();
    for child in tok.children.iter() {
        write_tokens(buf, child.clone());
    }
}

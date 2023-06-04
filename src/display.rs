use crate::{
    item::{AlphaMemoryItem, ConstantTest, Token, Wme},
    node::{AlphaMemoryNode, BetaMemoryNode, JoinNode, NegativeNode, Node, ProductionNode},
    RcCell, Rete,
};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter, Result, Write},
    rc::Rc,
};

impl Display for AlphaMemoryNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let items = self.items.iter().fold(String::new(), |mut acc, el| {
            write!(
                acc,
                "{},",
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
            Node::Negative(negative) => {
                write!(f, "{}", negative)
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

impl Display for NegativeNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "Negative {{ id: {} }}", self.id,)
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
            "Token {{ id: {}, parent: {:?}, wme: {:?}, node: {}, children: {:?} }}",
            self.id,
            self.parent.as_ref().map(|t| t
                .try_borrow()
                .map_or("borrowed".to_string(), |t| t.id.to_string())),
            self.wme
                .as_ref()
                .map(|wme| wme.try_borrow().map_or([0, 0, 0], |wme| wme.fields)), // TODO
            self.node
                .try_borrow()
                .map_or("borrowed".to_string(), |n| n.id().to_string()),
            self.children
                .iter()
                .map(|tok| tok
                    .try_borrow()
                    .map_or("borrowed".to_string(), |t| t.id.to_string()))
                .collect::<Vec<_>>()
        )
    }
}

impl Display for Wme {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "WME {{ id: {}, fields: {:?}, tokens: {:?} }}",
            self.id,
            self.fields,
            self.tokens
                .iter()
                .map(|t| t
                    .try_borrow()
                    .map_or("borrowed".to_string(), |t| t.id.to_string()))
                .collect::<Vec<_>>()
        )
    }
}

impl Rete {
    pub fn print_to_file(&self, path: &str) -> std::result::Result<(), std::io::Error> {
        let mut buf = match std::fs::read_to_string("printed") {
            Ok(buf) => buf,
            Err(_) => {
                if path.contains('/') {
                    std::fs::create_dir_all(path.split_at(path.rfind('/').unwrap()).0).unwrap();
                }
                std::fs::write(path, "").unwrap();
                std::fs::read_to_string(path).unwrap()
            }
        };
        writeln!(buf, "WMES\n").unwrap();
        write_wmes(&mut buf, &self.working_memory);
        writeln!(buf, "\nTOKENS\n").unwrap();
        write_tokens(&mut buf, self.dummy_top_token.clone());
        writeln!(buf, "\nBETA NETWORK\n").unwrap();
        write_beta_network(&mut buf, self.dummy_top_node.clone());
        writeln!(buf, "\nALPHA NETWORK\n").unwrap();
        write_alpha_network(&mut buf, &self.constant_tests);
        writeln!(buf, "\nPRODUCTIONS\n").unwrap();
        write_productions(&mut buf, &self.productions);
        std::fs::write(path, buf)?;
        Ok(())
    }
}

fn write_productions(buf: &mut String, prods: &HashMap<usize, RcCell<Node>>) {
    let mut items = prods.iter().collect::<Vec<_>>();
    items.sort_by(|a, b| a.0.cmp(b.0));
    for (_, prod_node) in items {
        writeln!(buf, "{}", prod_node.borrow()).unwrap();
    }
}

fn write_wmes(buf: &mut String, wmes: &HashMap<usize, RcCell<Wme>>) {
    let mut items = wmes.iter().collect::<Vec<_>>();
    items.sort_by(|a, b| a.0.cmp(b.0));
    for (_, wme) in items {
        writeln!(buf, "{}", wme.borrow()).unwrap();
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
        "{}{}, refs: {}",
        " ".repeat(tok.parent.as_ref().map_or(0, |p| p.borrow().id * 2)),
        tok,
        Rc::strong_count(&token)
    )
    .unwrap();
    for child in tok.children.iter() {
        write_tokens(buf, child.clone());
    }
}

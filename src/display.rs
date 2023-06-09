use crate::{
    item::{AlphaMemoryItem, Condition, ConstantTest, NegativeJoinResult, Production, Token, Wme},
    node::{
        AlphaMemoryNode, BetaMemoryNode, JoinNode, NccNode, NccPartnerNode, NegativeNode, Node,
        ProductionNode, ReteNode,
    },
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
                    .map_or_else(|_| "borrowed".to_string(), |el| el.to_string())
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
            self.alpha_memory
                .try_borrow()
                .map_or("borrowed".to_string(), |a| a.id.to_string()),
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
            Node::Ncc(ncc) => {
                write!(f, "{}", ncc)
            }
            Node::NccPartner(partner) => {
                write!(f, "{}", partner)
            }
        }
    }
}

impl Display for ProductionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "Prod {{ id: {}, production: {} }}",
            self.id, self.production
        )
    }
}

impl Display for Production {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "Production {{ id: {}, conditions: {} }}",
            self.id,
            self.conditions.iter().fold(String::new(), |mut acc, el| {
                write!(acc, "{}", el).unwrap();
                acc
            })
        )
    }
}

impl Display for Condition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut buf = String::new();
        match self {
            Condition::Positive { test } => {
                write!(buf, "P[")?;
                for (i, t) in test.iter().enumerate() {
                    let delim = if i == 2 { "" } else { " - " };
                    match t {
                        crate::item::ConditionTest::Constant(id) => write!(buf, "C_{id}{delim}")?,
                        crate::item::ConditionTest::Variable(id) => write!(buf, "V_{id}{delim}")?,
                    }
                }
                write!(buf, "], ")?;
            }
            Condition::Negative { test } => {
                write!(buf, "N[")?;
                for (i, t) in test.iter().enumerate() {
                    let delim = if i == 2 { "" } else { " - " };
                    match t {
                        crate::item::ConditionTest::Constant(id) => write!(buf, "C_{id}{delim}")?,
                        crate::item::ConditionTest::Variable(id) => write!(buf, "V_{id}{delim}")?,
                    }
                }
                write!(buf, "], ")?;
            }
            Condition::NegativeConjunction { subconditions } => {
                write!(buf, "NCC{{")?;
                for t in subconditions {
                    write!(buf, "{t}")?
                }
                write!(buf, "}}, ")?;
            }
        }
        write!(f, "{buf}")
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
            "Join {{ id: {}, parent: {:?}, children: {:?}, tests: {:?} }}",
            self.id,
            self.parent.borrow().id(),
            self.children
                .iter()
                .map(|node| node.borrow().id())
                .collect::<Vec<_>>(),
            self.tests.iter().collect::<Vec<_>>()
        )
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "Token {{ id: {}, parent: {:?}, wme: {:?}, node: {}, children: {:?}, neg_join_res: {}, ncc_res: {}, owner: {} }}",
            self.id,
            self.parent.as_ref().map(|t| t
                .try_borrow()
                .map_or("borrowed".to_string(), |t| t.id.to_string())),
            self.wme
                .as_ref()
                .map(|wme| wme.try_borrow().map_or([0, 0, 0], |wme| wme.fields)),
            self.node
                .try_borrow()
                .map_or("borrowed".to_string(), |n| n.id().to_string()),
            self.children
                .iter()
                .map(|tok| tok
                    .try_borrow()
                    .map_or("borrowed".to_string(), |t| t.id.to_string()))
                .collect::<Vec<_>>(),
            self.negative_join_results.iter().fold(String::new(), |mut acc, el| {
                write!(
                    acc,
                    "{},",
                    el.try_borrow()
                        .map_or_else(|_| "borrowed".to_string(), |el| el.id.to_string())
                )
                .unwrap();
                acc
            }),
            self.ncc_results.iter().fold(String::new(), |mut acc, el| {
                write!(
                    acc,
                    "{},",
                    el.try_borrow()
                        .map_or_else(|_| "borrowed".to_string(), |el| el.id.to_string())
                )
                .unwrap();
                acc
            }),
            self.owner.as_ref().map_or("None".to_string(), |o|{
                o.try_borrow()
                .map_or("borrowed".to_string(), |n| n.id.to_string())}
            )
        )
    }
}

impl Display for NegativeJoinResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "NegativeJoinRes {{ id: {}, owner: {}, wme: {} }}",
            self.id,
            self.owner
                .try_borrow()
                .map_or("borrowed".to_string(), |o| o.id.to_string()),
            self.wme
                .try_borrow()
                .map_or("borrowed".to_string(), |wme| format!("{}", wme))
        )
    }
}
impl Display for NccNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "NCC {{ id: {}, parent: {}, children: {:?}, items: {:?}, partner: {:?} }}",
            self.id,
            self.parent
                .try_borrow()
                .map_or("borrowed".to_string(), |p| p.id().to_string()),
            self.children
                .iter()
                .map(|t| t
                    .try_borrow()
                    .map_or("borrowed".to_string(), |t| t.id().to_string()))
                .collect::<Vec<_>>(),
            self.items
                .iter()
                .map(|t| t
                    .try_borrow()
                    .map_or("borrowed".to_string(), |t| t.id.to_string()))
                .collect::<Vec<_>>(),
            self.partner.as_ref().map(|p| {
                p.try_borrow()
                    .map_or("borrowed".to_string(), |p| p.id().to_string())
            })
        )
    }
}

impl Display for NccPartnerNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "NCC Partner {{ id: {}, conjucts: {} parent: {}, ncc: {}, new_results: {:?} }}",
            self.id,
            self.number_of_conjucts,
            self.parent
                .try_borrow()
                .map_or("borrowed".to_string(), |p| p.id().to_string()),
            self.ncc_node
                .try_borrow()
                .map_or("borrowed".to_string(), |n| n.id().to_string()),
            self.new_results
                .iter()
                .map(|t| t
                    .try_borrow()
                    .map_or("borrowed".to_string(), |t| t.id.to_string()))
                .collect::<Vec<_>>(),
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
        write_beta_network(&mut buf, &self.dummy_top_node);
        writeln!(buf, "\nALPHA NETWORK\n").unwrap();
        write_alpha_network(&mut buf, &self.constant_tests);
        writeln!(buf, "\nPRODUCTIONS\n").unwrap();
        write_productions(&mut buf, &self.productions);
        std::fs::write(path, buf)?;
        Ok(())
    }
}

fn write_productions(buf: &mut String, prods: &HashMap<usize, ReteNode>) {
    let mut items = prods.iter().collect::<Vec<_>>();
    items.sort_by(|a, b| a.0.cmp(b.0));
    for (_, prod_node) in items {
        let count = Rc::strong_count(prod_node);
        writeln!(buf, "{}, refs: {}", prod_node.borrow(), count).unwrap();
    }
}

fn write_wmes(buf: &mut String, wmes: &HashMap<usize, RcCell<Wme>>) {
    let mut items = wmes.iter().collect::<Vec<_>>();
    items.sort_by(|a, b| a.0.cmp(b.0));
    for (_, wme) in items {
        let count = Rc::strong_count(wme);
        writeln!(buf, "{}, refs: {}", wme.borrow(), count).unwrap();
    }
}

fn write_alpha_network(buf: &mut String, alpha: &HashMap<ConstantTest, RcCell<AlphaMemoryNode>>) {
    let mut items = alpha.iter().collect::<Vec<_>>();
    items.sort_by(|a, b| a.1.borrow().id.cmp(&b.1.borrow().id));
    for (test, alpha) in items {
        let count = Rc::strong_count(alpha);
        writeln!(buf, "{:?} :\n{}\n, refs: {}", test, alpha.borrow(), count).unwrap();
    }
}

fn write_beta_network(buf: &mut String, node: &ReteNode) {
    let count = Rc::strong_count(node);
    let node = node.borrow();
    writeln!(
        buf,
        "{}{}, refs: {}",
        " ".repeat(node.parent().as_ref().map_or(0, |p| p.borrow().id() * 2)),
        node,
        count
    )
    .unwrap();

    let Some(children) = node.children() else { return; };

    for child in children {
        write_beta_network(buf, child)
    }
}

fn write_tokens(buf: &mut String, token: RcCell<Token>) {
    let count = Rc::strong_count(&token);
    let tok = token.borrow();
    writeln!(
        buf,
        "{}{}, refs: {}",
        " ".repeat(tok.parent.as_ref().map_or(0, |p| p.borrow().id * 2)),
        tok,
        count
    )
    .unwrap();
    for child in tok.children.iter() {
        write_tokens(buf, child.clone());
    }
}

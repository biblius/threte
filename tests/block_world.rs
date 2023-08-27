use std::sync::atomic::AtomicUsize;
use std::sync::mpsc::{channel, Receiver, Sender};
use threte::engine::{Engine, Rule};
use threte::rete::item::Wme;
use threte::{
    engine::IntoWmes,
    rete::{
        id::reset,
        item::{Condition, ConditionTest, Production},
        Rete,
    },
};

const B1: usize = 1;
const B2: usize = 2;
const B3: usize = 3;
const B4: usize = 4;
const B5: usize = 5;
const B6: usize = 6;

const ON: usize = 10;
const COLOR: usize = 11;
const LEFT_OF: usize = 12;
const ID: usize = 13;

const RED: usize = 20;
const MAIZE: usize = 21;
const BLUE: usize = 23;

const TABLE: usize = 25;

const W1: [usize; 3] = [B1, ON, B2];
const W2: [usize; 3] = [B1, ON, B3];
const W3: [usize; 3] = [B1, COLOR, RED];
const W4: [usize; 3] = [B2, ON, TABLE];
const W5: [usize; 3] = [B2, LEFT_OF, B3];
const W6: [usize; 3] = [B2, COLOR, BLUE];
const W7: [usize; 3] = [B3, LEFT_OF, B4];
const W8: [usize; 3] = [B3, ON, TABLE];
const W9: [usize; 3] = [B3, COLOR, RED];

const V_X: ConditionTest = ConditionTest::Variable(0);
const V_Y: ConditionTest = ConditionTest::Variable(1);
const V_Z: ConditionTest = ConditionTest::Variable(2);
const V_A: ConditionTest = ConditionTest::Variable(3);
const V_B: ConditionTest = ConditionTest::Variable(4);

const C_ON: ConditionTest = ConditionTest::Constant(ON);
const C_LEFT_OF: ConditionTest = ConditionTest::Constant(LEFT_OF);
const C_COLOR: ConditionTest = ConditionTest::Constant(COLOR);
const C_RED: ConditionTest = ConditionTest::Constant(RED);
const C_MAIZE: ConditionTest = ConditionTest::Constant(MAIZE);
const C_BLUE: ConditionTest = ConditionTest::Constant(BLUE);
const C_ID: ConditionTest = ConditionTest::Constant(ID);
const C_TABLE: ConditionTest = ConditionTest::Constant(TABLE);

const C1: Condition = Condition::new_positive([V_X, C_ON, V_Y]);
const C2: Condition = Condition::new_positive([V_Y, C_LEFT_OF, V_Z]);
const C3: Condition = Condition::new_positive([V_Z, C_COLOR, C_RED]);
const C4: Condition = Condition::new_positive([V_A, C_COLOR, C_MAIZE]);
const C5: Condition = Condition::new_positive([V_B, C_COLOR, C_BLUE]);
const C6: Condition = Condition::new_positive([V_Z, C_ON, C_TABLE]);

#[derive(Debug, Hash)]
struct Block {
    /// The internal ID used by the rete
    rete_id: usize,

    /// The external ID used in the engine
    id: usize,

    /// [ON], [LEFT_OF]
    positions: Vec<Position>,

    /// [RED], [MAIZE], [BLUE]
    color: Color, // WME attribute 11
}

static IDENTIFIER: AtomicUsize = AtomicUsize::new(0);

impl IntoWmes for Block {
    fn id(&self) -> usize {
        self.id
    }

    fn to_wmes(&self) -> Vec<Wme> {
        let mut wmes = vec![Wme::new([self.rete_id, ID, self.id])];

        for pos in self.positions.iter() {
            match pos {
                Position::On(on) => wmes.push(Wme::new([self.rete_id, ON, *on])),
                Position::LeftOf(of) => wmes.push(Wme::new([self.rete_id, LEFT_OF, *of])),
                Position::Table => wmes.push(Wme::new([self.rete_id, ON, TABLE])),
            }
        }

        match self.color {
            Color::Red => wmes.push(Wme::new([self.rete_id, COLOR, RED])),
            Color::Maize => wmes.push(Wme::new([self.rete_id, COLOR, MAIZE])),
            Color::Blue => wmes.push(Wme::new([self.rete_id, COLOR, BLUE])),
        }

        wmes
    }
}

#[derive(Debug, Hash)]
enum Position {
    On(usize),
    LeftOf(usize),
    Table,
}

#[derive(Debug, Hash)]
enum Color {
    Red,
    Maize,
    Blue,
}

#[test]
fn engine() {
    let block1 = Block {
        rete_id: 1 << (usize::BITS / 2),
        id: 1,
        positions: vec![Position::Table],
        color: Color::Red,
    };

    let block2 = Block {
        rete_id: 2 << (usize::BITS / 2),
        id: 2,
        positions: vec![Position::LeftOf(block1.rete_id)],
        color: Color::Maize,
    };

    let block3 = Block {
        rete_id: 3 << (usize::BITS / 2),
        id: 3,
        positions: vec![Position::On(block2.rete_id)],
        color: Color::Blue,
    };

    let rule1 = Rule {
        conditions: vec![C1, C2, C3],
        production: Box::new(|e, b| {
            println!("Rule 1 works!");
            let el = e.elements.get(&b[0]);
            dbg!(el);
        }),
        bindings: vec![1, 2],
    };

    let rule2 = Rule {
        conditions: vec![C1, C2, C6],
        production: Box::new(|_e, _| {
            println!("Rule 2 works!");
        }),
        bindings: vec![],
    };

    let mut engine = Engine::default();

    engine.add_rule(rule1);
    engine.add_rule(rule2);

    engine.add_element(block1);
    engine.add_element(block2);
    engine.add_element(block3);

    engine.activate_productions();
}

fn productions(tx: Sender<usize>) -> Vec<Production> {
    vec![
        Production::new(&[C1, C2, C3], tx.clone()),
        Production::new(&[C1, C2, C4, C5], tx.clone()),
        Production::new(&[C1, C2, C3, C4], tx),
    ]
}

fn wmes() -> Vec<[usize; 3]> {
    vec![W1, W2, W3, W4, W5, W6, W7, W8, W9]
}

#[test]
fn add_productions() {
    let mut rete = Rete::default();
    let (tx, _rx) = channel();
    for p in productions(tx) {
        rete.add_production(p);
    }

    assert_eq!(rete.constant_tests.len(), 5);

    reset()
}

#[test]
fn add_productions_and_wmes() {
    let mut rete = Rete::default();
    let (tx, _rx) = channel();
    for p in productions(tx) {
        rete.add_production(p);
    }

    for wme in wmes() {
        rete.add_wme(Wme::new(wme));
    }

    reset()
}

#[test]
fn add_wme_then_prod() {
    let (tx, rx) = channel();
    let production_one = Production::new(&[C1, C2, C3], tx.clone());
    let production_two = Production::new(&[C1, C2, C4, C5], tx);

    let mut rete = Rete::default();

    rete.add_wme(Wme::new([B1, ON, B2]));
    rete.add_wme(Wme::new([B2, LEFT_OF, B3]));
    rete.add_wme(Wme::new([B3, COLOR, RED]));

    // Production should activate here

    rete.add_wme(Wme::new([B2, ON, B3]));
    rete.add_wme(Wme::new([B3, LEFT_OF, B4]));
    rete.add_wme(Wme::new([B5, COLOR, MAIZE]));
    rete.add_wme(Wme::new([B6, COLOR, BLUE]));

    // And here, 2 in total

    rete.add_production(production_one);
    rete.add_production(production_two);

    // TODO: I have no clue why this is 3
    assert_production_set_size(rx, 3);

    reset()
}

#[test]
fn add_productions_and_wmes_then_remove() {
    let (tx, rx) = channel();
    let production_one = Production::new(&[C1, C2, C3], tx.clone());
    let production_two = Production::new(&[C1, C2, C4, C5], tx);

    let mut rete = Rete::default();

    let prod_id1 = rete.add_production(production_one);
    rete.print_to_file("add_productions_and_wmes_then_remove/10_first_prod.txt")
        .unwrap();

    let prod_id2 = rete.add_production(production_two);
    rete.print_to_file("add_productions_and_wmes_then_remove/11_second_prod.txt")
        .unwrap();

    let id1 = rete.add_wme(Wme::new([B1, ON, B2]));
    rete.print_to_file("add_productions_and_wmes_then_remove/12_add_first_wme.txt")
        .unwrap();
    let id2 = rete.add_wme(Wme::new([B2, LEFT_OF, B3]));
    rete.print_to_file("add_productions_and_wmes_then_remove/13_add_second_wme.txt")
        .unwrap();
    let id3 = rete.add_wme(Wme::new([B3, COLOR, RED]));
    rete.print_to_file("add_productions_and_wmes_then_remove/14_add_third_wme.txt")
        .unwrap();
    let id4 = rete.add_wme(Wme::new([B2, ON, B3]));
    rete.print_to_file("add_productions_and_wmes_then_remove/15_add_fourth_wme.txt")
        .unwrap();
    let id5 = rete.add_wme(Wme::new([B3, LEFT_OF, B4]));
    rete.print_to_file("add_productions_and_wmes_then_remove/16_add_fifth_wme.txt")
        .unwrap();
    let id6 = rete.add_wme(Wme::new([B5, COLOR, MAIZE]));
    rete.print_to_file("add_productions_and_wmes_then_remove/17_add_sixth_wme.txt")
        .unwrap();
    let id7 = rete.add_wme(Wme::new([B6, COLOR, BLUE]));

    rete.print_to_file("add_productions_and_wmes_then_remove/18_add_all_wmes.txt")
        .unwrap();

    rete.remove_wme(id1);
    rete.remove_wme(id2);
    rete.remove_wme(id3);
    rete.remove_wme(id4);
    rete.remove_wme(id5);
    rete.remove_wme(id6);
    rete.remove_wme(id7);

    rete.print_to_file("add_productions_and_wmes_then_remove/19_remove_all_wmes.txt")
        .unwrap();

    assert!(rete.working_memory.is_empty());
    assert_eq!(rete.dummy_top_token.borrow().children().len(), 1);

    rete.remove_production(prod_id1);
    rete.remove_production(prod_id2);

    rete.print_to_file("add_productions_and_wmes_then_remove/20_remove_all_prods.txt")
        .unwrap();

    assert!(rete.productions.is_empty());
    assert!(rete.dummy_top_node.borrow().children().is_empty());

    // TODO Also no idea why 3, fml
    assert_production_set_size(rx, 3);

    reset()
}

fn assert_production_set_size(rx: Receiver<usize>, size: usize) {
    let mut i = 0;
    while rx.try_recv().is_ok() {
        i += 1;
    }

    assert_eq!(i, size);
}

#[test]
fn simple_wme_removal() {
    let mut rete = Rete::default();

    let id = rete.add_wme(Wme::new([1, 2, 3]));

    rete.remove_wme(id);

    assert!(rete.working_memory.is_empty());

    reset();
}

#[test]
fn wme_removal_with_tokens() {
    const X: usize = 0;
    const Y: usize = 1;
    const Z: usize = 2;

    const W1: [usize; 3] = [X, ON, Y];
    const W2: [usize; 3] = [Y, LEFT_OF, Z];

    let mut rete = Rete::default();

    let (tx, rx) = channel();
    let production = Production::new(&[C1, C2, C3], tx);

    rete.add_production(production);

    let id1 = rete.add_wme(Wme::new(W1));
    let id2 = rete.add_wme(Wme::new(W2));

    rete.print_to_file("wme_removal_with_tokens/initial.txt")
        .unwrap();

    rete.remove_wme(id2);

    rete.print_to_file("wme_removal_with_tokens/remove_first_wme.txt")
        .unwrap();

    rete.remove_wme(id1);

    assert_production_set_size(rx, 0);

    assert!(rete.working_memory.is_empty());
    assert_eq!(rete.dummy_top_token.borrow().children().len(), 1);

    rete.print_to_file("wme_removal_with_tokens/remove_second_wme.txt")
        .unwrap();

    reset();
}

#[test]
fn simple_production_removal() {
    let mut rete = Rete::default();

    let (tx, _rx) = channel();
    let production = Production::new(&[C1, C2, C3], tx);

    let id = rete.add_production(production);
    let removed = rete.remove_production(id);

    assert!(removed);
    assert!(rete.productions.is_empty());
    assert!(rete.dummy_top_node.borrow().children().is_empty());

    reset();
}

#[test]
fn production_removal_with_tokens() {
    const X: usize = 0;
    const Y: usize = 1;
    const Z: usize = 2;

    const W1: [usize; 3] = [X, ON, Y];
    const W2: [usize; 3] = [Y, LEFT_OF, Z];

    let mut rete = Rete::default();

    let (tx, rx) = channel();
    let production = Production::new(&[C1, C2, C3], tx);

    let id = rete.add_production(production);

    let id1 = rete.add_wme(Wme::new(W1));
    let id2 = rete.add_wme(Wme::new(W2));

    rete.print_to_file("production_removal_with_tokens/initial.txt")
        .unwrap();

    rete.remove_wme(id2);

    rete.print_to_file("production_removal_with_tokens/remove_first_wme.txt")
        .unwrap();

    rete.remove_wme(id1);

    rete.print_to_file("production_removal_with_tokens/remove_second_wme.txt")
        .unwrap();

    rete.remove_production(id);

    rete.print_to_file("production_removal_with_tokens/remove_production.txt")
        .unwrap();

    assert!(rete.working_memory.is_empty());
    assert!(rete.dummy_top_token.borrow().children().is_empty());
    assert!(rete.productions.is_empty());
    assert_production_set_size(rx, 0);

    reset();
}

#[test]
fn production_removal_with_similar_productions() {
    const X: usize = 0;
    const Y: usize = 1;
    const Z: usize = 2;

    const C1: Condition = Condition::new_positive([V_X, C_ON, V_Y]);
    const C2: Condition = Condition::new_positive([V_Y, C_LEFT_OF, V_Z]);
    const C3: Condition = Condition::new_positive([V_Z, C_COLOR, C_RED]);
    const C4: Condition = Condition::new_positive([V_Z, C_COLOR, C_BLUE]);

    const W1: [usize; 3] = [X, ON, Y];
    const W2: [usize; 3] = [Y, LEFT_OF, Z];

    let mut rete = Rete::default();

    let (tx, rx) = channel();
    let production_1 = Production::new(&[C1, C2, C3], tx.clone());
    let production_2 = Production::new(&[C1, C2, C4], tx);

    let prod_id1 = rete.add_production(production_1);
    let prod_id2 = rete.add_production(production_2);

    let id1 = rete.add_wme(Wme::new(W1));
    let id2 = rete.add_wme(Wme::new(W2));

    rete.print_to_file("production_removal_with_similar_productions/initial.txt")
        .unwrap();

    rete.remove_wme(id2);

    rete.print_to_file("production_removal_with_similar_productions/remove_first_wme.txt")
        .unwrap();

    rete.remove_wme(id1);

    rete.print_to_file("production_removal_with_similar_productions/remove_second_wme.txt")
        .unwrap();

    rete.remove_production(prod_id2);

    rete.print_to_file("production_removal_with_similar_productions/remove_production_2.txt")
        .unwrap();

    assert_eq!(rete.productions.len(), 1);

    rete.remove_production(prod_id1);

    rete.print_to_file("production_removal_with_similar_productions/remove_production_1.txt")
        .unwrap();

    assert!(rete.working_memory.is_empty());
    assert!(rete.dummy_top_token.borrow().children().is_empty());
    assert!(rete.productions.is_empty());
    assert_production_set_size(rx, 0);

    reset();
}

#[test]
fn add_remove_negative_node() {
    const X: usize = 0;
    const Y: usize = 1;
    const Z: usize = 2;

    const C1: Condition = Condition::new_positive([V_X, C_ON, V_Y]);
    const C2: Condition = Condition::new_positive([V_Y, C_LEFT_OF, V_Z]);
    const C3: Condition = Condition::new_negative([V_Z, C_COLOR, C_RED]);

    const W1: [usize; 3] = [X, ON, Y];
    const W2: [usize; 3] = [Z, COLOR, BLUE];
    const W3: [usize; 3] = [Y, LEFT_OF, Z];

    let mut rete = Rete::default();

    let (tx, rx) = channel();
    let production = Production::new(&[C1, C2, C3], tx);

    let prod_id1 = rete.add_production(production);
    assert_eq!(rete.productions.len(), 1);

    rete.add_wme(Wme::new(W1));
    rete.add_wme(Wme::new(W2));
    rete.add_wme(Wme::new(W3));

    rete.print_to_file("add_remove_negative_node/initial.txt")
        .unwrap();

    rete.remove_production(prod_id1);

    rete.print_to_file("add_remove_negative_node/remove_production.txt")
        .unwrap();

    assert!(rete.dummy_top_token.borrow().children().is_empty());
    assert!(rete.productions.is_empty());

    assert_production_set_size(rx, 1);

    reset();
}

#[test]
fn add_remove_ncc_node() {
    const X: usize = 0;
    const Y: usize = 1;
    const Z: usize = 2;

    const C1: Condition = Condition::new_positive([V_X, C_ON, V_Y]);
    const C2: Condition = Condition::new_positive([V_Y, C_LEFT_OF, V_Z]);
    let nccs = vec![
        Condition::new_positive([V_Z, C_COLOR, C_RED]),
        Condition::new_positive([V_Z, C_LEFT_OF, V_A]),
    ];
    let nc_3: Condition = Condition::new_ncc(nccs);

    const W1: [usize; 3] = [Z, COLOR, BLUE];
    const W2: [usize; 3] = [X, ON, Y];
    const W3: [usize; 3] = [Y, LEFT_OF, Z];

    let mut rete = Rete::default();
    let (tx, rx) = channel();
    let production = Production::new(&[C1, C2, nc_3], tx);

    let prod_id1 = rete.add_production(production);
    assert_eq!(rete.productions.len(), 1);

    rete.print_to_file("add_remove_ncc_node/0_initial.txt")
        .unwrap();

    rete.add_wme(Wme::new(W1));
    rete.print_to_file("add_remove_ncc_node/1_first_wme.txt")
        .unwrap();

    rete.add_wme(Wme::new(W2));
    rete.print_to_file("add_remove_ncc_node/2_second_wme.txt")
        .unwrap();

    rete.add_wme(Wme::new(W3));
    rete.print_to_file("add_remove_ncc_node/3_third_wme.txt")
        .unwrap();

    rete.remove_production(prod_id1);

    rete.print_to_file("add_remove_ncc_node/4_remove_production.txt")
        .unwrap();

    assert!(rete.dummy_top_token.borrow().children().is_empty());
    assert!(rete.productions.is_empty());

    assert_production_set_size(rx, 1);

    reset();
}

#[test]
fn add_remove_single_ncc() {
    const X: usize = 0;
    const Y: usize = 1;

    let nccs = vec![
        Condition::new_positive([V_X, C_COLOR, C_RED]),
        Condition::new_positive([V_X, C_LEFT_OF, V_Y]),
    ];
    let nc: Condition = Condition::new_ncc(nccs);

    const W1: [usize; 3] = [X, COLOR, RED];
    const W2: [usize; 3] = [X, LEFT_OF, Y];

    println!("hello");

    let mut rete = Rete::default();

    let (tx, _rx) = channel();
    let production = Production::new(&[nc], tx);

    rete.add_wme(Wme::new(W1));
    rete.add_wme(Wme::new(W2));
    rete.print_to_file("add_remove_single_ncc/0_add_wmes.txt")
        .unwrap();

    let prod_id1 = rete.add_production(production);
    assert_eq!(rete.productions.len(), 1);

    rete.print_to_file("add_remove_single_ncc/1_add_production.txt")
        .unwrap();

    rete.remove_production(prod_id1);

    rete.print_to_file("add_remove_single_ncc/2_remove_production.txt")
        .unwrap();

    assert!(rete.dummy_top_token.borrow().children().is_empty());
    assert!(rete.productions.is_empty());

    reset();
}

#[test]
fn ncc_complex() {
    const A: usize = 0;
    const B: usize = 1;
    const C: usize = 2;
    const D: usize = 3;

    // Every red block has a blue block on top of it can be rewritten as a check that there is no
    // red block that does not have a blue block on top of it.

    // The first positive condition is necessary in order to provide the NCC with proper
    // variable bindings. Otherwise the NCC will always do it's thing with the dummy token
    // which is no bueno.

    // The following conditions can be read: There does not exist a red block (c1)
    // that has no blue block on top of it (ncc)
    let c = Condition::new_positive([V_X, C_COLOR, C_RED]);
    let ncc = Condition::new_ncc(vec![
        Condition::new_positive([V_X, C_COLOR, C_RED]),
        Condition::new_ncc(vec![
            Condition::new_positive([V_Y, C_ON, V_X]),
            Condition::new_positive([V_Y, C_COLOR, C_BLUE]),
        ]),
    ]);

    const W1: [usize; 3] = [B, COLOR, RED];
    const W2: [usize; 3] = [A, COLOR, BLUE];
    const W3: [usize; 3] = [A, ON, B];
    const W4: [usize; 3] = [D, COLOR, RED];
    const W5: [usize; 3] = [C, COLOR, BLUE];
    const W6: [usize; 3] = [C, ON, D];

    let mut rete = Rete::default();

    let (tx, rx) = channel();
    let production = Production::new(&[c, ncc], tx);

    let prod_id1 = rete.add_production(production);
    assert_eq!(rete.productions.len(), 1);

    rete.print_to_file("ncc_complex/0_initial.txt").unwrap();

    let wme1 = rete.add_wme(Wme::new(W1));
    rete.print_to_file("ncc_complex/10_first_wme.txt").unwrap();

    let wme2 = rete.add_wme(Wme::new(W2));
    rete.print_to_file("ncc_complex/11_second_wme.txt").unwrap();

    let wme3 = rete.add_wme(Wme::new(W3));
    rete.print_to_file("ncc_complex/12_third_wme.txt").unwrap();

    let wme4 = rete.add_wme(Wme::new(W4));
    rete.print_to_file("ncc_complex/13_fourth_wme.txt").unwrap();

    let wme5 = rete.add_wme(Wme::new(W5));
    rete.print_to_file("ncc_complex/14_fifth_wme.txt").unwrap();

    let wme6 = rete.add_wme(Wme::new(W6));
    rete.print_to_file("ncc_complex/15_sixth_wme.txt").unwrap();

    rete.remove_wme(wme6);
    rete.print_to_file("ncc_complex/16_rem_sixth_wme.txt")
        .unwrap();

    rete.remove_wme(wme5);
    rete.print_to_file("ncc_complex/17_rem_fifth_wme.txt")
        .unwrap();

    rete.remove_wme(wme4);
    rete.print_to_file("ncc_complex/18_rem_fourth_wme.txt")
        .unwrap();

    rete.remove_wme(wme3);
    rete.print_to_file("ncc_complex/19_rem_third_wme.txt")
        .unwrap();

    rete.remove_wme(wme2);
    rete.print_to_file("ncc_complex/20_rem_second_wme.txt")
        .unwrap();

    rete.remove_wme(wme1);
    rete.print_to_file("ncc_complex/21_rem_first_wme.txt")
        .unwrap();

    rete.remove_production(prod_id1);

    rete.print_to_file("ncc_complex/22_remove_production.txt")
        .unwrap();

    assert!(rete.dummy_top_token.borrow().children().is_empty());
    assert!(rete.productions.is_empty());

    assert_production_set_size(rx, 2);

    reset();
}

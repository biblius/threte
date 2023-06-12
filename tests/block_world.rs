use threte::{
    id::reset,
    item::{Condition, ConditionTest, Production},
    Rete,
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

const C1: Condition = Condition::new_positive([V_X, C_ON, V_Y]);
const C2: Condition = Condition::new_positive([V_Y, C_LEFT_OF, V_Z]);
const C3: Condition = Condition::new_positive([V_Z, C_COLOR, C_RED]);
const C4: Condition = Condition::new_positive([V_A, C_COLOR, C_MAIZE]);
const C5: Condition = Condition::new_positive([V_B, C_COLOR, C_BLUE]);

fn productions() -> Vec<Production> {
    vec![
        Production {
            id: 9001,
            conditions: vec![C1, C2, C3],
        },
        Production {
            id: 9002,
            conditions: vec![C1, C2, C4, C5],
        },
        Production {
            id: 9003,
            conditions: vec![C1, C2, C4, C3],
        },
    ]
}

fn wmes() -> Vec<[usize; 3]> {
    vec![W1, W2, W3, W4, W5, W6, W7, W8, W9]
}

#[test]
fn add_productions() {
    let mut rete = Rete::default();
    for p in productions() {
        rete.add_production(p);
    }

    assert_eq!(rete.constant_tests.len(), 5);

    reset()
}

#[test]
fn add_productions_and_wmes() {
    let mut rete = Rete::default();
    for p in productions() {
        rete.add_production(p);
    }

    for wme in wmes() {
        rete.add_wme(wme);
    }

    reset()
}

#[test]
fn add_productions_and_wmes_then_remove() {
    let production_one = Production {
        id: 9000,
        conditions: vec![C1, C2, C3],
    };
    let production_two = Production {
        id: 9001,
        conditions: vec![C1, C2, C4, C5],
    };

    let mut rete = Rete::default();

    let prod_id1 = rete.add_production(production_one);
    let prod_id2 = rete.add_production(production_two);

    let id1 = rete.add_wme([B1, ON, B2]);
    let id2 = rete.add_wme([B2, LEFT_OF, B3]);
    let id3 = rete.add_wme([B3, COLOR, RED]);
    let id4 = rete.add_wme([B2, ON, B3]);
    let id5 = rete.add_wme([B3, LEFT_OF, B4]);
    let id6 = rete.add_wme([B5, COLOR, MAIZE]);
    let id7 = rete.add_wme([B6, COLOR, BLUE]);

    rete.print_to_file("add_productions_and_wmes_then_remove/0_initial.txt")
        .unwrap();

    rete.remove_wme(id1);
    rete.remove_wme(id2);
    rete.remove_wme(id3);
    rete.remove_wme(id4);
    rete.remove_wme(id5);
    rete.remove_wme(id6);
    rete.remove_wme(id7);

    rete.print_to_file("add_productions_and_wmes_then_remove/1_wmes_removed.txt")
        .unwrap();

    println!("{}", rete.dummy_top_node.borrow().children().unwrap().len());
    println!("{}", rete.dummy_top_token.borrow().children().len());
    assert!(rete.working_memory.is_empty());
    assert_eq!(rete.dummy_top_token.borrow().children().len(), 1);

    rete.remove_production(prod_id1);
    rete.remove_production(prod_id2);

    assert!(rete.productions.is_empty());
    assert!(rete.dummy_top_node.borrow().children().is_none());

    reset()
}

#[test]
fn add_wme_then_prod() {
    let production_one = Production {
        id: 9000,
        conditions: vec![C1, C2, C3],
    };
    let production_two = Production {
        id: 9001,
        conditions: vec![C1, C2, C4, C5],
    };

    let mut rete = Rete::default();

    rete.add_wme([B1, ON, B2]);
    rete.add_wme([B2, LEFT_OF, B3]);
    rete.add_wme([B3, COLOR, RED]);
    rete.add_wme([B2, ON, B3]);
    rete.add_wme([B3, LEFT_OF, B4]);
    rete.add_wme([B5, COLOR, MAIZE]);
    rete.add_wme([B6, COLOR, BLUE]);

    rete.add_production(production_one);
    rete.add_production(production_two);

    reset()
}

#[test]
fn simple_wme_removal() {
    let mut rete = Rete::default();

    let id = rete.add_wme([1, 2, 3]);

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

    let production = Production {
        id: 9000,
        conditions: vec![C1, C2, C3],
    };

    rete.add_production(production);

    let id1 = rete.add_wme(W1);
    let id2 = rete.add_wme(W2);

    rete.print_to_file("wme_removal_with_tokens/initial.txt")
        .unwrap();

    rete.remove_wme(id2);

    rete.print_to_file("wme_removal_with_tokens/remove_first_wme.txt")
        .unwrap();

    rete.remove_wme(id1);

    assert!(rete.working_memory.is_empty());
    assert_eq!(rete.dummy_top_token.borrow().children().len(), 1);

    rete.print_to_file("wme_removal_with_tokens/remove_second_wme.txt")
        .unwrap();

    reset();
}

#[test]
fn simple_production_removal() {
    let mut rete = Rete::default();

    let production = Production {
        id: 9000,
        conditions: vec![C1, C2, C3],
    };

    let id = rete.add_production(production);

    let removed = rete.remove_production(id);

    assert!(removed);
    assert!(rete.productions.is_empty());
    assert!(rete.dummy_top_node.borrow().children().is_none());

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

    let production = Production {
        id: 9000,
        conditions: vec![C1, C2, C3],
    };

    let id = rete.add_production(production);

    let id1 = rete.add_wme(W1);
    let id2 = rete.add_wme(W2);

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

    let production_1 = Production {
        id: 9000,
        conditions: vec![C1, C2, C3],
    };
    let production_2 = Production {
        id: 9001,
        conditions: vec![C1, C2, C4],
    };

    let prod_id1 = rete.add_production(production_1);
    let prod_id2 = rete.add_production(production_2);

    let id1 = rete.add_wme(W1);
    let id2 = rete.add_wme(W2);

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

    let production_1 = Production {
        id: 9000,
        conditions: vec![C1, C2, C3],
    };

    let prod_id1 = rete.add_production(production_1);
    assert_eq!(rete.productions.len(), 1);

    rete.add_wme(W1);
    rete.add_wme(W2);
    rete.add_wme(W3);

    rete.print_to_file("add_remove_negative_node/initial.txt")
        .unwrap();

    rete.remove_production(prod_id1);

    rete.print_to_file("add_remove_negative_node/remove_production.txt")
        .unwrap();

    assert!(rete.dummy_top_token.borrow().children().is_empty());
    assert!(rete.productions.is_empty());

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

    let production_1 = Production {
        id: 9000,
        conditions: vec![C1, C2, nc_3],
    };

    let prod_id1 = rete.add_production(production_1);
    assert_eq!(rete.productions.len(), 1);

    rete.print_to_file("add_remove_ncc_node/0_initial.txt")
        .unwrap();

    rete.add_wme(W1);
    rete.print_to_file("add_remove_ncc_node/1_first_wme.txt")
        .unwrap();

    rete.add_wme(W2);
    rete.print_to_file("add_remove_ncc_node/2_second_wme.txt")
        .unwrap();

    rete.add_wme(W3);
    rete.print_to_file("add_remove_ncc_node/3_third_wme.txt")
        .unwrap();

    rete.remove_production(prod_id1);

    rete.print_to_file("add_remove_ncc_node/4_remove_production.txt")
        .unwrap();

    assert!(rete.dummy_top_token.borrow().children().is_empty());
    assert!(rete.productions.is_empty());

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

    let production_1 = Production {
        id: 9000,
        conditions: vec![nc],
    };

    rete.add_wme(W1);
    rete.add_wme(W2);
    rete.print_to_file("add_remove_single_ncc/0_add_wmes.txt")
        .unwrap();

    let prod_id1 = rete.add_production(production_1);
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
    // red block that does not have a blue block on top of it
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

    let production_1 = Production {
        id: 9000,
        conditions: vec![c, ncc],
    };

    let prod_id1 = rete.add_production(production_1);
    assert_eq!(rete.productions.len(), 1);

    rete.print_to_file("ncc_complex/0_initial.txt").unwrap();

    let wme1 = rete.add_wme(W1);
    rete.print_to_file("ncc_complex/10_first_wme.txt").unwrap();

    let wme2 = rete.add_wme(W2);
    rete.print_to_file("ncc_complex/11_second_wme.txt").unwrap();

    let wme3 = rete.add_wme(W3);
    rete.print_to_file("ncc_complex/12_third_wme.txt").unwrap();

    let wme4 = rete.add_wme(W4);
    rete.print_to_file("ncc_complex/13_fourth_wme.txt").unwrap();

    let wme5 = rete.add_wme(W5);
    rete.print_to_file("ncc_complex/14_fifth_wme.txt").unwrap();

    let wme6 = rete.add_wme(W6);
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

    reset();
}

use crate::rete::{
    item::{Condition, Production, Wme},
    Rete,
};
use std::{
    collections::HashMap,
    hash::Hash,
    sync::mpsc::{channel, Receiver, Sender},
};

pub type ProductionAction = Box<dyn Fn(&mut Engine, &[usize])>;

#[derive(Debug)]
pub struct EngineElement {
    /// Used for modifying WMEs
    rete_id: usize,

    ///
    fields: [usize; 3],
}

pub struct Engine {
    pub rete: Rete,
    pub elements: HashMap<usize, Vec<EngineElement>>,
    pub prod_sender: Sender<usize>,
    pub production_queue: Receiver<usize>,
    pub production_map: HashMap<usize, Rule>,
}

impl Default for Engine {
    fn default() -> Self {
        let (tx, rx) = channel();
        Self {
            rete: Rete::default(),
            elements: HashMap::new(),
            prod_sender: tx,
            production_queue: rx,
            production_map: HashMap::new(),
        }
    }
}

impl Engine {
    pub fn add_element<T: IntoWmes>(&mut self, element: T) {
        let engine_id = element.id();

        for wme in element.to_wmes() {
            let fields = wme.fields;
            let rete_id = self.rete.add_wme(wme);

            self.elements
                .entry(engine_id)
                .and_modify(|e| e.push(EngineElement { rete_id, fields }))
                .or_insert_with(|| vec![EngineElement { rete_id, fields }]);
        }
    }

    pub fn add_rule(&mut self, rule: Rule) {
        let rete_prod = Production::new(&rule.conditions, self.prod_sender.clone());

        self.production_map.insert(rete_prod.id, rule);

        self.rete.add_production(rete_prod);
    }

    pub fn activate_productions(&mut self) {
        while let Ok(id) = self.production_queue.try_recv() {
            let Some(rule) = self.production_map.remove(&id) else {
                continue;
            };
            let bindings = &rule.bindings;
            (rule.production)(self, bindings);
            self.production_map.insert(id, rule);
        }
    }
}

pub trait IntoWmes: Hash {
    fn id(&self) -> usize;
    fn to_wmes(&self) -> Vec<Wme>;
}

pub struct Rule {
    pub conditions: Vec<Condition>,

    pub production: ProductionAction,
    pub bindings: Vec<usize>,
}

/* rule! {
    "My rule"
    when {
        block: Block {
            id == 5,
        },
        other: Block {
            positions: contains(Position::On(block.id))
        }
    }
    then {
        modify(other) {
            color = Color::Red
        }
    }
}
 */

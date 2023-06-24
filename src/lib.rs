pub mod display;
pub mod id;
pub mod item;
pub mod node;

use crate::{
    item::{conditions_to_constant_tests, AlphaMemoryItem, DUMMY_TOKEN_ID},
    node::{BetaMemoryNode, JoinNode, NccNode, NccPartnerNode, ProductionNode},
};
use item::{Condition, ConstantTest, JoinTest, NegativeJoinResult, Production, Token, Wme};
use node::{AlphaMemoryNode, NegativeNode, Node, ReteNode};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug)]
pub enum Error {
    CondToConstantTest,
}

type RcCell<T> = Rc<RefCell<T>>;

trait IntoCell: Sized {
    fn to_cell(self) -> RcCell<Self> {
        Rc::new(RefCell::new(self))
    }
}

trait IntoNodeCell: Sized {
    fn to_node_cell(self) -> ReteNode;
}

#[derive(Debug)]
pub struct Rete {
    /// Token tree root
    pub dummy_top_token: RcCell<Token>,

    /// Beta network root
    pub dummy_top_node: ReteNode,

    /// Since every WME is represented as a triple, we only need to do 8 hash table look ups whenever one is added to the
    /// network to find possibly matching constant tests for it. This removes the need for a constant test network.
    pub constant_tests: HashMap<ConstantTest, RcCell<AlphaMemoryNode>>,

    /// Maps WME IDs to their corresponding elements for quick removal of tokens
    pub working_memory: HashMap<usize, RcCell<Wme>>,

    /// Maps WME IDs to Alpha Nodes that contain items which hold the WME
    pub wme_alphas: HashMap<usize, Vec<RcCell<AlphaMemoryNode>>>,

    /// Maps production IDs to their corresponding production nodes
    pub productions: HashMap<usize, ReteNode>,
}

impl Default for Rete {
    fn default() -> Self {
        Self::new()
    }
}

impl Rete {
    fn new() -> Self {
        let dummy_top_node = BetaMemoryNode::dummy();

        let dummy_top_token = Token::new_dummy(&dummy_top_node);

        dummy_top_node.borrow_mut().add_token(&dummy_top_token);

        Self {
            constant_tests: HashMap::new(),
            wme_alphas: HashMap::new(),
            working_memory: HashMap::new(),
            productions: HashMap::new(),
            dummy_top_node,
            dummy_top_token,
        }
    }

    pub fn add_wme(&mut self, elements: [usize; 3]) -> usize {
        let wme = Wme::new(elements);
        let id = wme.id;

        println!("-----------\nAdding WME {:?}\n-----------", wme);

        for element in wme.permutations() {
            let Some(memory) = self.constant_tests.get(&element) else { continue };

            // Index the memories that will hold this WME by its ID
            if let Some(memories) = self.wme_alphas.get_mut(&id) {
                memories.push(Rc::clone(memory));
            } else {
                self.wme_alphas.insert(id, vec![Rc::clone(memory)]);
            }

            let wme = wme.to_cell();
            self.working_memory.insert(id, Rc::clone(&wme));

            println!(
                "Found existing memory {} for element {:?}",
                memory.borrow().id,
                element
            );

            activate_alpha_memory(memory, &wme);

            return id;
        }

        println!("No memory found for WME {wme:?}, inserting to working memory");

        let wme = wme.to_cell();
        self.working_memory.insert(id, Rc::clone(&wme));

        id
    }

    pub fn remove_wme(&mut self, id: usize) {
        println!("Removing WME {id}");

        // Remove all items representing the wme from the alpha network
        if let Some(memories) = self.wme_alphas.remove(&id) {
            for memory in memories {
                println!("Removing WME {id} from alpha memory {}", memory.borrow().id);
                memory
                    .borrow_mut()
                    .items
                    .retain(|item| item.borrow().wme.borrow().id != id);

                // The alpha memory just became empty, left unlink the corresponding
                // join node
                if memory.borrow().items.is_empty() {
                    for successor in memory.borrow().successors.iter() {
                        if let Node::Join(join) = &*successor.borrow() {
                            join.parent.borrow_mut().remove_child(join.id)
                        }
                    }
                }
            }
        }

        let Some(wme) = self.working_memory.remove(&id) else { return; };

        println!("Removing WME {} from working memory", wme.borrow());

        let mut wme = wme.borrow_mut();
        let tokens = std::mem::take(&mut wme.tokens);
        let n_join_results = std::mem::take(&mut wme.negative_join_results);

        drop(wme);

        // Remove all tokens representing the wme
        for token in tokens.into_iter() {
            Token::delete_self_and_descendants(token)
        }

        // Remove all associated negative join results from the result's owner
        // and trigger left activation to test for new absence
        for result in n_join_results {
            let result = result.borrow();

            let is_empty = result.owner.borrow_mut().remove_join_result(result.id);

            if is_empty {
                for child in result.owner.borrow().node().borrow().children() {
                    activate_left(child, &result.owner, None);
                }
            }
        }
    }

    pub fn add_production(&mut self, production: Production) -> usize {
        println!(
            "------------\nAdding production {}\n------------",
            production.id
        );

        let id = production.id;
        let conditions = &production.conditions;

        let current_node = self.build_or_share_network_for_conditions(
            &Rc::clone(&self.dummy_top_node),
            conditions,
            &mut vec![],
        );

        let production = ProductionNode::new(production, &current_node).to_node_cell();

        self.productions.insert(id, Rc::clone(&production));

        current_node.borrow_mut().add_child(&production);

        update_new_node_with_matches_from_above(&production);

        id
    }

    pub fn remove_production(&mut self, id: usize) -> bool {
        let Some(production) = self.productions.remove(&id) else { return false; };

        println!(
            "------------\nRemoving production {}\n------------",
            production.borrow()
        );

        let mut tests = vec![];
        {
            let Node::Production(prod) = &*production.borrow() else { panic!("Rust no longer works")};
            conditions_to_constant_tests(&mut tests, &prod.production.conditions);
        }

        self.delete_node_and_unused_ancestors(production, &tests);

        true
    }

    fn build_or_share_network_for_conditions<'a>(
        &mut self,
        parent: &ReteNode,
        conditions: &'a [Condition],
        earlier_conds: &mut Vec<&'a Condition>,
    ) -> ReteNode {
        assert!(!conditions.is_empty(), "LHS of production cannot be empty");

        println!(
            "Building/sharing network for conditions with parent {}",
            parent.borrow()
        );

        let mut current_node = Rc::clone(parent);

        for condition in conditions.iter() {
            println!("Processing condition {:?}", condition);

            match condition {
                Condition::Positive { .. } => {
                    current_node = build_or_share_beta_memory_node(&current_node);
                    let tests = get_join_tests_from_condition(condition, earlier_conds);
                    let alpha_memory = self.build_or_share_alpha_memory_node(condition);
                    current_node = build_or_share_join_node(&current_node, &alpha_memory, tests);
                }
                Condition::Negative { .. } => {
                    let tests = get_join_tests_from_condition(condition, earlier_conds);
                    let alpha_memory = self.build_or_share_alpha_memory_node(condition);
                    current_node =
                        build_or_share_negative_node(&current_node, &alpha_memory, tests);
                }
                Condition::NegativeConjunction { subconditions } => {
                    current_node =
                        self.build_or_share_ncc_nodes(&current_node, subconditions, earlier_conds)
                }
            }

            earlier_conds.push(condition);
        }

        current_node
    }

    fn build_or_share_ncc_nodes<'a>(
        &mut self,
        parent: &ReteNode,
        subconditions: &'a [Condition],
        earlier_conds: &mut Vec<&'a Condition>,
    ) -> ReteNode {
        let subnet_bottom =
            self.build_or_share_network_for_conditions(parent, subconditions, earlier_conds);

        let ncc_node = NccNode::new(parent).to_node_cell();

        println!("Built {}", ncc_node.borrow());

        let partner =
            NccPartnerNode::new(&ncc_node, &subnet_bottom, subconditions.len()).to_node_cell();

        println!("Built {}", partner.borrow());

        if let Node::Ncc(ncc) = &mut *ncc_node.borrow_mut() {
            ncc.partner = Some(Rc::clone(&partner))
        };

        subnet_bottom.borrow_mut().add_child(&partner);

        parent.borrow_mut().add_child(&ncc_node);

        // Update the NCC first, otherwise lots of matches would get mixed together
        // in the partner's `new_results` buffer
        update_new_node_with_matches_from_above(&ncc_node);
        update_new_node_with_matches_from_above(&partner);

        ncc_node
    }

    fn build_or_share_alpha_memory_node(
        &mut self,
        condition: &Condition,
    ) -> RcCell<AlphaMemoryNode> {
        let constant_test = ConstantTest::try_from(condition).unwrap();

        println!("Searching for constant test {constant_test:?}");

        // Check whether an alpha memory like this exists
        if let Some(alpha_mem) = self.constant_tests.get(&constant_test) {
            println!("Shared {}", alpha_mem.borrow());
            return Rc::clone(alpha_mem);
        }

        // Alpha memory not found, create new one and insert into map
        let am = AlphaMemoryNode::new().to_cell();

        self.constant_tests.insert(constant_test, Rc::clone(&am));

        println!(
            "Indexing constant test {constant_test:?} to AM {}",
            am.borrow()
        );

        println!("Searching for matching WMEs for {constant_test:?}");

        for (_, wme) in self.working_memory.iter() {
            let _wme = wme.borrow();
            if constant_test.matches(&_wme) {
                println!("Found match: {_wme} for constant test {constant_test:?}");
                let a_mems = self
                    .wme_alphas
                    .entry(_wme.id)
                    .or_insert_with(|| vec![Rc::clone(&am)]);
                a_mems
                    .iter()
                    .for_each(|mem| activate_alpha_memory(mem, wme))
            }
        }

        am
    }

    fn delete_node_and_unused_ancestors(
        &mut self,
        node: ReteNode,
        constant_tests: &[ConstantTest],
    ) {
        // Used to avoid a mutable reference when recursively deleting node references from tokens
        enum NodeRemove {
            Join {
                id: usize,
                parent: ReteNode,
                left_linked: bool,
                right_linked: bool,
                alpha_mem: RcCell<AlphaMemoryNode>,
            },
            Beta(Vec<RcCell<Token>>),
            Negative {
                right_linked: bool,
                alpha_mem: RcCell<AlphaMemoryNode>,
                tokens: Vec<RcCell<Token>>,
            },
            Ncc {
                partner: ReteNode,
                ncc_tokens: Vec<RcCell<Token>>,
            },
            NccPartner {
                ncc_partner_tokens: Vec<RcCell<Token>>,
            },
            Production,
        }
        println!("Deleting Node {}", node.borrow());

        let remove_result = match &mut *node.borrow_mut() {
            Node::Join(ref mut join) => NodeRemove::Join {
                id: join.id,
                parent: Rc::clone(&join.parent),
                left_linked: join.left_linked,
                right_linked: join.right_linked,
                alpha_mem: Rc::clone(&join.alpha_mem),
            },
            Node::Beta(beta) => NodeRemove::Beta(std::mem::take(&mut beta.items)),
            Node::Production(_) => NodeRemove::Production,
            Node::Negative(negative) => NodeRemove::Negative {
                right_linked: negative.right_linked,
                alpha_mem: Rc::clone(&negative.alpha_mem),
                tokens: std::mem::take(&mut negative.items),
            },
            Node::Ncc(ncc) => {
                let partner = ncc.partner.take().unwrap();
                NodeRemove::Ncc {
                    partner,
                    ncc_tokens: std::mem::take(&mut ncc.items),
                }
            }
            Node::NccPartner(partner) => NodeRemove::NccPartner {
                ncc_partner_tokens: std::mem::take(&mut partner.new_results),
            },
        };

        match remove_result {
            NodeRemove::Join {
                id,
                parent,
                alpha_mem,
                left_linked,
                right_linked,
            } => {
                if right_linked {
                    alpha_mem
                        .borrow_mut()
                        .successors
                        .retain(|child| child.borrow().id() != node.borrow().id());
                }

                if left_linked {
                    if let Node::Beta(ref mut beta) = *parent.borrow_mut() {
                        beta.all_children.retain(|child| child.borrow().id() != id);
                    }
                }

                if parent.borrow().all_children().is_empty() {
                    self.delete_node_and_unused_ancestors(parent, constant_tests)
                }

                if alpha_mem.borrow().successors.is_empty() {
                    println!("Deleting Alpha Mem {}", alpha_mem.borrow());
                    alpha_mem.borrow_mut().items.clear();

                    for test in constant_tests {
                        let Some(mem) = self.constant_tests.get(test) else { continue; };
                        if mem.borrow().successors.is_empty() {
                            self.constant_tests.remove(test);
                        }
                    }
                }
            }
            NodeRemove::Beta(tokens) => {
                for token in tokens.into_iter() {
                    Token::delete_self_and_descendants(token)
                }
            }
            NodeRemove::Negative {
                right_linked,
                alpha_mem,
                tokens,
            } => {
                for token in tokens.into_iter() {
                    Token::delete_self_and_descendants(token)
                }

                if right_linked {
                    alpha_mem
                        .borrow_mut()
                        .successors
                        .retain(|child| child.borrow().id() != node.borrow().id());
                }

                if alpha_mem.borrow().successors.is_empty() {
                    println!("Deleting Alpha Mem {}", alpha_mem.borrow());
                    alpha_mem.borrow_mut().items.clear();

                    for test in constant_tests {
                        let Some(mem) = self.constant_tests.get(test) else { continue; };
                        if mem.borrow().successors.is_empty() {
                            self.constant_tests.remove(test);
                        }
                    }
                }
            }
            NodeRemove::Ncc {
                partner,
                ncc_tokens,
            } => {
                self.delete_node_and_unused_ancestors(partner, constant_tests);
                for token in ncc_tokens.into_iter() {
                    Token::delete_self_and_descendants(token)
                }
            }
            NodeRemove::NccPartner { ncc_partner_tokens } => {
                for token in ncc_partner_tokens.into_iter() {
                    Token::delete_self_and_descendants(token)
                }
            }
            NodeRemove::Production => {}
        }

        // Remove this node from its parent and remove the parent if it
        // was the last child
        if let Some(parent) = node.borrow().parent() {
            {
                let id = node.borrow().id();
                parent.borrow_mut().remove_child(id);
            }
            println!(
                "Parent node {} remaining children {:?}",
                parent.borrow().id(),
                parent
                    .borrow()
                    .children()
                    .iter()
                    .map(|n| n.borrow().id())
                    .collect::<Vec<_>>()
            );
            if parent.borrow().children().is_empty() {
                self.delete_node_and_unused_ancestors(parent, constant_tests);
            }
        }
    }
}

/// This procedures is triggered whenever a new production enters the system and its job is to find
/// potential existing matches for the newly created production by checking the parent node
/// and propagating activations if matches are found.
fn update_new_node_with_matches_from_above(node: &ReteNode) {
    println!("Updating node {}", node.borrow());

    let Some(parent) = node.borrow().parent() else { return; };

    println!("Updating parent {}", parent.borrow());

    // This avoids keeping the mutable borrow when recursively right activating the join node
    let children = match *parent.borrow_mut() {
        Node::Beta(ref beta) => {
            beta.items.iter().for_each(|token| {
                activate_left(node, token, None);
            });
            return;
        }
        Node::Negative(ref mut negative) => {
            negative.items.iter().for_each(|token| {
                if !token.borrow().contains_join_results() {
                    activate_left(node, token, None);
                }
            });
            return;
        }
        Node::Ncc(ref mut ncc) => {
            ncc.items.iter().for_each(|token| {
                if !token.borrow().contains_ncc_results() {
                    activate_left(node, token, None);
                }
            });
            return;
        }
        Node::Join(ref mut join) => std::mem::replace(&mut join.children, vec![Rc::clone(node)]),
        Node::NccPartner(_) => panic!("NCC partner node cannot have children"),
        Node::Production(_) => panic!("Production node cannot have children"),
    };

    // The rest is for the join node procedure since we cannot keep a mutable borrow when
    // activating

    let items = match *parent.borrow() {
        Node::Join(ref join) => std::mem::take(&mut join.alpha_mem.borrow_mut().items),
        _ => unreachable!("Rust no longer works"),
    };

    for item in items.iter() {
        activate_right(&parent, &item.borrow().wme);
    }

    let Node::Join(ref mut join) = *parent.borrow_mut() else { panic!("Rust no longer works") };

    join.alpha_mem.borrow_mut().items = items;

    join.children = children;
}

/// Activation of alpha memories cause them to right activate join nodes which in turn makes
/// the join nodes search through their beta memories and perform join tests on already existing tokens,
/// further propagating left activations if they find new matches.
fn activate_alpha_memory(alpha_mem_node: &RcCell<AlphaMemoryNode>, wme: &RcCell<Wme>) {
    let item = AlphaMemoryItem::new(wme, alpha_mem_node).to_cell();

    // Insert new item at the head of the node's items and take the successors to avoid
    // having an active borrow when re-linking nodes
    let successors = {
        let mut alpha_mem = alpha_mem_node.borrow_mut();
        println!("-> Activating Alpha Node: {alpha_mem}");
        alpha_mem.items.push(Rc::clone(&item));
        std::mem::take(&mut alpha_mem.successors)
    };

    // As successors are added to alpha memories, they will be descendents of previous
    // join nodes. In order to mitigate token duplication, desdendents must be right activated
    // before ancestors
    for successor in successors.into_iter().rev() {
        {
            alpha_mem_node
                .borrow_mut()
                .successors
                .push(Rc::clone(&successor));
        }
        activate_right(&successor, wme);
    }
}

/// A right activation of a `JoinNode` will cause it to iterate through its
/// parent's tokens and perform a [join_test] on each one and the given WME.
///
/// For every test that passes, a left activation is triggered on each of the node's
/// children.
///
/// Right activations are caused by [AlphaMemoryNode]s when [WME][Wme]s are changed or
/// when new [WME][Wme]s enter the network
fn activate_right(node: &ReteNode, wme: &RcCell<Wme>) {
    println!(
        "➡️ Right activating {} {}",
        node.borrow()._type(),
        node.borrow().id()
    );

    let (mut l_link, mut r_link) = (true, true);

    if let Node::Join(ref join) = *node.borrow() {
        l_link = join.left_linked;
        r_link = join.right_linked;
        // The activation comes from an alpha memory that just became non-empty so we need
        // to relink the join node to the beta network.
        if !join.left_linked {
            println!(
                "🔗 Relinking {} to beta memory {}",
                join.id,
                join.parent.borrow().id()
            );
            join.parent.borrow_mut().add_child(node);
            l_link = true;
            // Subsequently if the beta is empty, we need to right unlink the node
            if join.parent.borrow().tokens().is_empty() {
                println!("💥 Right unlinking {}", join.id);
                join.alpha_mem
                    .borrow_mut()
                    .successors
                    .retain(|suc| suc.borrow().id() != join.id);
                r_link = false;
            }
        }
    }
    if let Node::Join(ref mut join) = *node.borrow_mut() {
        join.left_linked = l_link;
        join.right_linked = r_link;
    }

    match *node.borrow() {
        Node::Join(ref join_node) => {
            if let Node::Beta(ref parent) = *join_node.parent.borrow() {
                for token in parent.items.iter() {
                    if join_test(&join_node.tests, token, &wme.borrow()) {
                        join_node.children.iter().for_each(|child| {
                            activate_left(child, token, Some(wme));
                        })
                    }
                }
            }
        }
        Node::Negative(ref negative_node) => {
            for token in negative_node.items.iter() {
                if join_test(&negative_node.tests, token, &wme.borrow()) {
                    // This check is done to see if the token's state has changed. If it previously
                    // had negative join results and is now empty, it means we must delete all its children
                    // since the partial matches are no longer valid.
                    if !token.borrow().contains_join_results() {
                        let children = std::mem::take(token.borrow_mut().children_mut());
                        Token::delete_descendants(children);
                    }
                    let join_result = NegativeJoinResult::new(token, wme).to_cell();
                    token.borrow_mut().add_join_result(&join_result);
                    wme.borrow_mut()
                        .negative_join_results
                        .push(Rc::clone(&join_result))
                }
            }
        }
        Node::Beta(_) => unreachable!("Beta memory nodes are never right activated"),
        Node::Production(_) => unreachable!("Production nodes are never right activated"),
        Node::Ncc(_) => unreachable!("NCC nodes are never right activated"),
        Node::NccPartner(_) => unreachable!("NCC Partner nodes are never right activated"),
    }
}

/// Left activations occur when partial matches are found for a production's conditions and WMEs in the working memory.
///
/// Left activating Beta nodes causes them to create and store tokens for the match and propagate the left activation to their children, i.e.
/// Join nodes, with the newly created token.
///
/// Left activating Join nodes causes them to perform join tests with the given token and the WME from their alpha memory, and if successful propagate
/// the left activation to their children.
///
/// Left activating Negative nodes causes them to create and store tokens for the match and perform join tests with the created token
/// and the WME from their alpha memory. If any of the items pass the test, the node will not further propagate the
/// activation, because it means a condition is true which should be false.
///
/// Left activating production nodes causes them to execute whatever is specified in their production.
///
/// Because of the borrow assblaster, this function returns a boolean indicating whether or not
/// the node being activated should remain in the children list of the caller. If the function returns `false`
/// the node should be removed from the list. Consequently, whenever a memory node has to activate its children
/// it does so through a `retain` call.
fn activate_left(node: &ReteNode, parent_token: &RcCell<Token>, wme: Option<&RcCell<Wme>>) -> bool {
    // Relink the appropriate nodes to their corresponding alpha mems if they are unlinked
    if !node.borrow().is_right_linked() && matches!(&*node.borrow(), Node::Join(_)) {
        Node::relink_to_alpha_mem(node);
    }

    // Left unlink the node if there are no more items in its alpha memory.
    // This check will always be triggered when left activating from one of three node types:
    // beta, negative and ncc.
    if let Node::Join(ref mut join) = *node.borrow_mut() {
        if join.alpha_mem.borrow().items.is_empty() {
            // This is the only case this function will return false, since only join nodes
            // can be left unlinked. If the join's items are empty, we return early
            // since it only propagates the activation if it finds a match in its alpha mem's items.
            join.left_linked = false;
            return false;
        }
    }

    if !node.borrow().is_right_linked() && matches!(&*node.borrow(), Node::Negative(_)) {
        Node::relink_to_alpha_mem(node);
    }

    match &mut *node.borrow_mut() {
        Node::Beta(ref mut beta_node) => {
            println!("⬅️ Left activating beta {}", beta_node.id,);

            let new_token = Token::new_beta(node, parent_token, wme);

            beta_node.items.push(Rc::clone(&new_token));

            beta_node
                .children
                .retain(|child| activate_left(child, &new_token, None));

            true
        }
        Node::Join(ref mut join_node) => {
            println!("⬅️ Left activating join {}", join_node.id);

            for item in join_node.alpha_mem.borrow().items.iter() {
                if join_test(&join_node.tests, parent_token, &item.borrow().wme.borrow()) {
                    for child in join_node.children.iter() {
                        activate_left(child, parent_token, Some(&item.borrow().wme));
                    }
                }
            }
            true
        }
        Node::Negative(negative_node) => {
            let new_token = Token::new_negative(node, parent_token, wme);

            println!(
                "⬅️ Left activating negative {} and appending token {}",
                negative_node.id,
                new_token.borrow().id()
            );

            negative_node.items.push(Rc::clone(&new_token));

            for item in negative_node.alpha_mem.borrow().items.iter() {
                if join_test(
                    &negative_node.tests,
                    &new_token,
                    &item.borrow().wme.borrow(),
                ) {
                    let join_result =
                        NegativeJoinResult::new(&new_token, &item.borrow().wme).to_cell();

                    new_token.borrow_mut().add_join_result(&join_result);

                    if let Some(wme) = wme {
                        wme.borrow_mut()
                            .negative_join_results
                            .push(Rc::clone(&join_result));
                    }
                }
            }

            // Negative nodes propagate left activations only if no tokens passed its join tests
            if !new_token.borrow().contains_join_results() {
                negative_node
                    .children
                    .retain(|child| activate_left(child, &new_token, None));
            }
            true
        }
        Node::Ncc(ncc_node) => {
            println!("⬅️ Left activating ncc {}", ncc_node.id,);
            let new_token = Token::new_ncc(node, parent_token, wme);

            ncc_node.items.push(Rc::clone(&new_token));

            if let Node::NccPartner(ncc_partner) =
                &mut *ncc_node.partner.as_ref().unwrap().borrow_mut()
            {
                println!(
                    "Checking NCC partner {} for new results (len {})",
                    ncc_partner.id,
                    ncc_partner.new_results.len()
                );
                for result in std::mem::take(&mut ncc_partner.new_results) {
                    result.borrow_mut().set_owner(&new_token);
                    new_token.borrow_mut().add_ncc_result(&result)
                }
            }

            if !new_token.borrow().contains_ncc_results() {
                ncc_node
                    .children
                    .retain(|child| activate_left(child, &new_token, None));
            }

            true
        }
        Node::NccPartner(ncc_partner) => {
            println!(
                "⬅️ Left activating ncc partner {} with parent token {}",
                ncc_partner,
                parent_token.borrow()
            );
            let ncc_node = &ncc_partner.ncc_node;
            let new_result = Token::new_ncc(node, parent_token, wme);

            let mut owners_token = Some(Rc::clone(parent_token));
            let mut owners_wme = wme.cloned();

            for _ in 0..ncc_partner.number_of_conjucts {
                owners_wme = owners_token.as_ref().unwrap().borrow().wme().cloned();
                let parent = owners_token.as_ref().unwrap().borrow().parent().cloned();
                owners_token = parent;
            }

            println!(
                "Current owner token {:?}",
                owners_token.as_ref().map(|t| t.borrow().id())
            );
            println!(
                "Current owner WME {:?}",
                owners_wme.as_ref().map(|t| t.borrow().id)
            );

            if let Node::Ncc(ncc) = &*ncc_node.borrow() {
                if let Some(owner) = ncc.items.iter().find(|token| {
                    let token = token.borrow();
                    token.parent() == owners_token.as_ref() && token.wme() == owners_wme.as_ref()
                }) {
                    println!(
                        "Found existing owner token {}",
                        owners_token.as_ref().unwrap().borrow()
                    );
                    let mut owner = owner.borrow_mut();
                    owner.add_ncc_result(&new_result);
                    let children = std::mem::take(owner.children_mut());
                    drop(owner);
                    Token::delete_descendants(children)
                } else {
                    println!("No owner token found for {}", new_result.borrow());
                    // There was no appropriate owner token already in the NCC's memory. This means
                    // the subnetwork was activated for a new match for the preceding conditions,
                    // and `new_result` emerged from the bottom, but the NCC node hasn't been
                    // activated for the new match yet.
                    ncc_partner.new_results.push(new_result)
                }
            }

            true
        }
        Node::Production(p_node) => {
            println!(
                "====================\nProduction node activated! {p_node}\n===================="
            );

            true
        }
    }
}

fn build_or_share_beta_memory_node(parent: &ReteNode) -> ReteNode {
    println!("Building/sharing beta node with parent {}", parent.borrow());

    // Look for an existing beta node to share
    for child in parent.borrow().children() {
        if let Node::Beta(ref beta) = *child.borrow() {
            println!("Shared {beta}");
            return Rc::clone(child);
        }
    }

    let new = BetaMemoryNode::new(Some(Rc::clone(parent))).to_node_cell();

    println!("Built {}", new.borrow());

    parent.borrow_mut().add_child(&new);

    update_new_node_with_matches_from_above(&new);

    new
}

fn build_or_share_join_node(
    parent: &ReteNode,
    alpha_memory: &RcCell<AlphaMemoryNode>,
    tests: Vec<JoinTest>,
) -> ReteNode {
    // Look for an existing join node to share
    for child in parent.borrow().all_children() {
        if let Node::Join(node) = &*child.borrow() {
            if node.tests.as_slice() == tests && *node.alpha_mem.borrow() == *alpha_memory.borrow()
            {
                println!("Sharing {node}");
                return Rc::clone(child);
            }
        }
    }

    let mut new = JoinNode::new(parent, alpha_memory, tests);

    new.nearest_ancestor = find_ancestor_with_same_amem(parent, alpha_memory);

    let new = new.to_node_cell();

    parent.borrow_mut().add_child(&new);
    if let Node::Beta(ref mut beta) = &mut *parent.borrow_mut() {
        beta.all_children.push(Rc::clone(&new))
    }

    // Add the newly created node to the alpha memory successors
    alpha_memory.borrow_mut().successors.push(Rc::clone(&new));

    // Right unlink if the parent beta memory is empty
    let parent = new.borrow().parent().unwrap();
    if parent.borrow().tokens().is_empty() {
        alpha_memory
            .borrow_mut()
            .successors
            .retain(|suc| suc.borrow().id() != new.borrow().id());
        if let Node::Join(join) = &mut *new.borrow_mut() {
            println!("💥 Right unlinking {}", join.id);
            join.right_linked = false;
        }
        // Left unlink if the parent alpha memory is empty
    } else if alpha_memory.borrow().items.is_empty() {
        parent.borrow_mut().remove_child(new.borrow().id());
        if let Node::Join(join) = &mut *new.borrow_mut() {
            println!("💥 Left unlinking {}", join.id);
            join.left_linked = false;
        }
    }

    println!("Built {}", new.borrow());

    new
}

fn find_ancestor_with_same_amem(
    node: &ReteNode,
    alpha_memory: &RcCell<AlphaMemoryNode>,
) -> Option<ReteNode> {
    if node.borrow().is_dummy() {
        return None;
    }

    match *node.borrow() {
        Node::Join(ref join) if &join.alpha_mem == alpha_memory => Some(Rc::clone(node)),
        Node::Negative(ref neg) if &neg.alpha_mem == alpha_memory => Some(Rc::clone(node)),
        Node::Ncc(ref ncc) => {
            let partner_parent = ncc.partner.as_ref().unwrap().borrow().parent().unwrap();
            find_ancestor_with_same_amem(&partner_parent, alpha_memory)
        }
        _ => return find_ancestor_with_same_amem(&node.borrow().parent().unwrap(), alpha_memory),
    }
}

fn build_or_share_negative_node(
    parent: &ReteNode,
    alpha_memory: &RcCell<AlphaMemoryNode>,
    tests: Vec<JoinTest>,
) -> ReteNode {
    for child in parent.borrow().children() {
        if let Node::Negative(node) = &*child.borrow() {
            if *node.alpha_mem.borrow() == *alpha_memory.borrow() && node.tests == tests {
                println!("Sharing {node}");
                return Rc::clone(child);
            }
        }
    }

    let mut new = NegativeNode::new(parent, alpha_memory, tests);

    new.nearest_ancestor = find_ancestor_with_same_amem(parent, alpha_memory);

    let new = new.to_node_cell();

    println!("Built {}", new.borrow());
    {
        alpha_memory.borrow_mut().successors.push(Rc::clone(&new));
    }

    parent.borrow_mut().add_child(&new);

    update_new_node_with_matches_from_above(&new);

    // The right unlink procedure checks whether the node has any items
    new.borrow().right_unlink();
    if let Node::Negative(ref mut neg) = *new.borrow_mut() {
        neg.right_linked = false;
    }

    new
}

/// Perform variable binding consistency tests for each test in `tests` with the given `token` and `wme`.
///
/// [Join tests][JoinTest] are stored by join and negative nodes and are executed whenever those node are activated.
fn join_test(tests: &[JoinTest], token: &RcCell<Token>, wme: &Wme) -> bool {
    println!(
        "Performing join tests on {tests:?} with WME {} {:?} and token {}",
        wme.id,
        wme.fields,
        token.borrow().id()
    );

    for test in tests.iter() {
        let parent = Token::nth_parent(Rc::clone(token), test.distance_to_wme);

        // If the tokens are pointing to the dummy token they immediatelly get a pass
        if parent.borrow().id() == DUMMY_TOKEN_ID {
            println!("Join test successful");
            return true;
        }

        let parent = parent.borrow();

        // If there is no WME on the token, it represents a negative node on the token
        // which should return false since the args are not equal??
        let Some(wme2) = &parent.wme() else { return false; };

        let wme2 = wme2.borrow();
        println!("Comparing WME {:?} from token {}", wme2.fields, parent.id());

        let current_value = wme[test.arg_one];
        let previous_value = wme2[test.arg_two];

        println!(
            "Testing Current WME {:?} with Previous {:?}, {current_value} != {previous_value}",
            wme.id, wme2.id,
        );

        if current_value != previous_value {
            return false;
        }
    }

    println!("Join test successful");
    true
}

fn get_join_tests_from_condition(
    condition: &Condition,
    earlier_conds: &[&Condition],
) -> Vec<JoinTest> {
    let mut result = vec![];

    println!(
        "Creating join tests from {:?} and earlier {:?}",
        condition, earlier_conds
    );

    let current_condition_num = earlier_conds.len();

    for (current_idx, var) in condition.variables() {
        let Some((distance, prev_idx)) = earlier_conds
            .iter()
            .enumerate()
            .rev()
            .find_map(|(idx, cond)| {
                // We do not care about variable bindings in previous negative conditions
                let Condition::Positive { .. } = cond else {
                    println!("Condition is negative, returning"); 
                    return None;
                };
                cond.variables().find_map(|(cond_idx, v)|
                if v == var {
                    Some((idx + 1, cond_idx))
                } else {
                    None
                }
            )})
        else {
            continue;
        };

        let test = JoinTest {
            arg_one: current_idx,
            distance_to_wme: current_condition_num - distance,
            arg_two: prev_idx,
        };

        result.push(test)
    }

    println!("Created join tests {:?}", result);
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::item::ConditionTest;

    #[test]
    fn it_works() {
        let mut rete = Rete::new();
        let conditions = Vec::from([Condition::new_positive([
            ConditionTest::Variable(1),
            ConditionTest::Constant(2),
            ConditionTest::Variable(3),
        ])]);
        rete.add_production(Production { id: 1, conditions });
        rete.add_wme([1, 2, 3]);
    }

    #[test]
    fn join_test_to_condition() {
        use ConditionTest::*;
        let condition = Condition::new_positive([Variable(2), Variable(3), Variable(4)]);
        let previous = &[
            &Condition::new_positive([Variable(1), Variable(1), Variable(2)]),
            &Condition::new_positive([Variable(5), Variable(6), Variable(3)]),
        ];
        let test_nodes = get_join_tests_from_condition(&condition, previous);

        assert_eq!(
            test_nodes[0],
            JoinTest {
                arg_one: 0,
                distance_to_wme: 1,
                arg_two: 2
            }
        );

        assert_eq!(
            test_nodes[1],
            JoinTest {
                arg_one: 1,
                distance_to_wme: 0,
                arg_two: 2
            }
        );

        let c = Condition::new_positive([Variable(1), Constant(0), Variable(2)]);
        let (c1, c2, c3) = (
            &Condition::new_positive([Variable(3), Constant(1), Variable(5)]),
            &Condition::new_positive([Variable(1), Constant(0), Variable(7)]),
            &Condition::new_positive([Variable(6), Constant(0), Variable(7)]),
        );
        let earlier = vec![c1, c2, c3];

        let result = get_join_tests_from_condition(&c, &earlier);

        assert_eq!(
            result[0],
            JoinTest {
                arg_one: 0,
                distance_to_wme: 1,
                arg_two: 0
            }
        );

        let c = Condition::new_positive([Variable(1), Constant(0), Variable(2)]);
        let (c1, c2, c3) = (
            &Condition::new_positive([Variable(3), Constant(1), Variable(5)]),
            &Condition::new_positive([Variable(2), Constant(0), Variable(7)]),
            &Condition::new_positive([Variable(6), Constant(0), Variable(1)]),
        );
        let earlier = vec![c1, c2, c3];

        let result = get_join_tests_from_condition(&c, &earlier);

        assert_eq!(
            result[0],
            JoinTest {
                arg_one: 0,
                distance_to_wme: 0,
                arg_two: 2
            }
        );
        assert_eq!(
            result[1],
            JoinTest {
                arg_one: 2,
                distance_to_wme: 1,
                arg_two: 0
            }
        );
    }

    #[test]
    fn nth_parent_works() {
        let beta = BetaMemoryNode::new(None);

        let wme = Wme::new([1, 2, 3]).to_cell();

        let node = beta.to_node_cell();

        let daddy = Token::new_dummy(&node);

        let child_token_one = Token::new_beta(&node, &daddy, Some(&wme));

        let child_token_two = Token::new_beta(&node, &child_token_one, Some(&wme));

        let child_token_three = Token::new_beta(&node, &child_token_two, Some(&wme));

        child_token_two
            .borrow_mut()
            .children_mut()
            .push(Rc::clone(&child_token_three));

        child_token_one
            .borrow_mut()
            .children_mut()
            .push(Rc::clone(&child_token_two));

        daddy
            .borrow_mut()
            .children_mut()
            .push(child_token_one.clone());

        let parent = Token::nth_parent(child_token_three, 2);

        assert_eq!(parent.borrow().id(), child_token_one.borrow().id());
        assert_eq!(
            parent.borrow().children().len(),
            child_token_one.borrow().children().len()
        );
    }
}

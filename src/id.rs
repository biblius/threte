use std::sync::atomic::AtomicUsize;

static ALPHA_NODE_ID_GENERATOR: AtomicUsize = AtomicUsize::new(0);
static BETA_NODE_ID_GENERATOR: AtomicUsize = AtomicUsize::new(0);
static TOKEN_ID_GENERATOR: AtomicUsize = AtomicUsize::new(0);
static ITEM_ID_GENERATOR: AtomicUsize = AtomicUsize::new(0);
static WME_ID_GENERATOR: AtomicUsize = AtomicUsize::new(0);

pub fn alpha_node_id() -> usize {
    ALPHA_NODE_ID_GENERATOR.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}

pub fn beta_node_id() -> usize {
    BETA_NODE_ID_GENERATOR.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}

pub fn token_id() -> usize {
    TOKEN_ID_GENERATOR.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}

pub fn item_id() -> usize {
    ITEM_ID_GENERATOR.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}

pub fn wme_id() -> usize {
    WME_ID_GENERATOR.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}

pub fn reset() {
    ALPHA_NODE_ID_GENERATOR.store(0, std::sync::atomic::Ordering::SeqCst);
    BETA_NODE_ID_GENERATOR.store(0, std::sync::atomic::Ordering::SeqCst);
    TOKEN_ID_GENERATOR.store(0, std::sync::atomic::Ordering::SeqCst);
    WME_ID_GENERATOR.store(0, std::sync::atomic::Ordering::SeqCst);
}

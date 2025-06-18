use super::function::Value;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Default, Clone, Copy, Hash, PartialEq, Eq)]
pub struct NodeId(usize);

impl NodeId {
    fn next(&mut self) -> Self {
        let idx = Self(self.0);
        self.0 += 1;

        idx
    }
}

#[derive(Debug)]
pub struct InterferenceGraph {
    next_idx: NodeId,
    nodes: HashMap<NodeId, Value>,
    edges: HashMap<NodeId, HashSet<NodeId>>,
}

impl InterferenceGraph {
    pub fn new() -> Self {
        Self {
            next_idx: NodeId::default(),
            nodes: HashMap::new(),
            edges: HashMap::new(),
        }
    }

    pub fn add_node(&mut self, node: Value) -> NodeId {
        let id = self.next_idx.next();

        self.nodes.insert(id, node);
        self.edges.insert(id, HashSet::new());

        id
    }

    pub fn add_node_with_node_id(&mut self, node: Value, id: NodeId) {
        assert!(self.nodes.insert(id, node).is_none());
        self.edges.insert(id, HashSet::new());
    }

    pub fn remove_node(&mut self, idx: NodeId) {
        self.nodes.remove(&idx);

        if let Some(neighbors) = self.edges.remove(&idx) {
            for neighbor in neighbors {
                if let Some(neighbors) = self.edges.get_mut(&neighbor) {
                    neighbors.remove(&idx);
                }
            }
        }
    }

    pub fn add_edge(&mut self, a: NodeId, b: NodeId) {
        assert!(a != b);

        self.edges.get_mut(&a).unwrap().insert(b);
        self.edges.get_mut(&b).unwrap().insert(a);
    }

    pub fn neighbors(&self, idx: NodeId) -> &HashSet<NodeId> {
        &self.edges[&idx]
    }

    pub fn get_node(&self, idx: NodeId) -> &Value {
        &self.nodes[&idx]
    }
}

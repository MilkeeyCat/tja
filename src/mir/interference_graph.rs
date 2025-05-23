use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug)]
pub struct InterferenceGraph<K: Copy + Eq + Hash>(HashMap<K, HashSet<K>>);

impl<K: Copy + Eq + Hash> InterferenceGraph<K> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn add_node(&mut self, node: K) {
        self.0.insert(node, HashSet::new());
    }

    pub fn remove_node(&mut self, node: &K) {
        if let Some(neighbors) = self.0.remove(node) {
            for neighbor in neighbors {
                if let Some(neighbors) = self.0.get_mut(&neighbor) {
                    neighbors.remove(node);
                }
            }
        }
    }

    pub fn add_edge(&mut self, a: K, b: K) {
        if a != b {
            self.0.get_mut(&a).unwrap().insert(b);
            self.0.get_mut(&b).unwrap().insert(a);
        }
    }

    pub fn neighbors(&self, node: &K) -> &HashSet<K> {
        &self.0[node]
    }
}

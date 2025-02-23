use super::{
    operands::{Destination, EffectiveAddress, Memory, OperandSize, Source},
    register::Register,
};
use crate::repr::{RegisterId, ty::Ty};
use std::collections::{HashMap, HashSet};

type Edges = HashSet<(RegisterId, RegisterId)>;

#[derive(Clone)]
pub enum Location {
    Register(Register),
    Address(EffectiveAddress),
}

impl Location {
    pub fn to_source(&self, size: OperandSize) -> Source {
        match self {
            Self::Register(r) => r.resize(size).into(),
            Self::Address(addr) => Source::Memory(Memory {
                effective_address: addr.clone(),
                size,
            }),
        }
    }

    pub fn to_dest(&self, size: OperandSize) -> Destination {
        match self {
            Self::Register(r) => r.resize(size).into(),
            Self::Address(addr) => Destination::Memory(Memory {
                effective_address: addr.clone(),
                size,
            }),
        }
    }
}

impl From<Register> for Location {
    fn from(value: Register) -> Self {
        Self::Register(value)
    }
}

impl From<EffectiveAddress> for Location {
    fn from(value: EffectiveAddress) -> Self {
        Self::Address(value)
    }
}

/// A weird looking graph coloring by simplification register allocator
pub struct Allocator {
    nodes: HashMap<RegisterId, Ty>,
    edges: Edges,
    registers: Vec<Register>,
    locations: HashMap<RegisterId, Location>,
}

impl Allocator {
    pub fn new(types: Vec<Ty>, edges: Edges, registers: Vec<Register>) -> Self {
        Self {
            nodes: types
                .into_iter()
                .enumerate()
                .map(|(i, ty)| (i, ty))
                .collect(),
            edges,
            registers,
            locations: HashMap::new(),
        }
    }

    fn remove_node(&mut self, node: &RegisterId) -> (Ty, HashSet<(RegisterId, RegisterId)>) {
        let edges: HashSet<_> = self
            .edges
            .iter()
            .filter(|(lhs, rhs)| lhs == node || rhs == node)
            .cloned()
            .collect();

        let ty = self.nodes.remove(&node).unwrap();
        edges.iter().for_each(|edge| _ = self.edges.remove(edge));

        (ty, edges)
    }

    fn add_node(&mut self, node: RegisterId, ty: Ty, edges: HashSet<(RegisterId, RegisterId)>) {
        self.nodes.insert(node, ty);
        self.edges.extend(edges.into_iter());
    }

    fn neighbors(&self, node: &RegisterId) -> Vec<&RegisterId> {
        self.edges
            .iter()
            .filter(|(lhs, rhs)| lhs == node || rhs == node)
            .map(|(lhs, rhs)| if lhs == node { rhs } else { lhs })
            .collect()
    }

    fn min(&self) -> Option<RegisterId> {
        self.nodes
            .iter()
            // ignore precolored nodes
            .filter(|(node, _)| !self.locations.contains_key(node))
            .map(|(node, _)| (*node, self.neighbors(node).len()))
            .min_by(|(_, a), (_, b)| a.cmp(b))
            .map(|(node, _)| node)
    }

    fn max(&self) -> Option<RegisterId> {
        self.nodes
            .iter()
            // ignore precolored nodes
            .filter(|(node, _)| !self.locations.contains_key(node))
            .map(|(node, _)| (*node, self.neighbors(node).len()))
            .max_by(|(_, a), (_, b)| a.cmp(b))
            .map(|(node, _)| node)
    }

    fn unique_register(&self, neighbors: &[&RegisterId]) -> Register {
        assert!(neighbors.len() < self.registers.len());
        for reg in &self.registers {
            let mut found = true;
            for neighbor in neighbors {
                if let Location::Register(r) = &self.locations[neighbor] {
                    if reg == r {
                        found = false;

                        break;
                    }
                }
            }

            if found {
                return *reg;
            }
        }
        unreachable!()
    }

    pub fn precolor(&mut self, node: RegisterId, location: Location) {
        self.locations.insert(node, location);
    }

    pub fn add_edge(&mut self, edge: (RegisterId, RegisterId)) {
        if !(self.edges.contains(&edge) || self.edges.contains(&(edge.1, edge.0))) {
            self.edges.insert(edge);
        }
    }

    pub fn create_node(&mut self, ty: Ty) -> RegisterId {
        let r = self.nodes.len();
        self.nodes.insert(r, ty);

        r
    }

    pub fn allocate(mut self) -> Vec<Location> {
        let mut stack: Vec<(RegisterId, Ty, Edges)> = Vec::new();

        while self.min().is_some() {
            let node = if self.min().unwrap() < self.registers.len() {
                self.min()
            } else {
                self.max()
            }
            .unwrap();
            let (ty, edges) = self.remove_node(&node);

            stack.push((node, ty, edges));
        }

        for (node, ty, edges) in stack.into_iter().rev() {
            if edges.len() < self.registers.len() {
                self.add_node(node, ty, edges);
                self.locations.insert(
                    node,
                    Location::Register(self.unique_register(&self.neighbors(&node))),
                );
            } else {
                // spill but still can check for registers
                panic!("Spilling is not implemented yet!");
            }
        }

        let mut operands: Vec<_> = self.locations.into_iter().collect();
        operands.sort_by(|(a, _), (b, _)| a.cmp(&b));
        operands.into_iter().map(|(_, operand)| operand).collect()
    }
}

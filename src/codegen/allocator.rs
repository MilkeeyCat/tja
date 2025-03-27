use super::{
    abi::Abi,
    operands::{Base, Destination, EffectiveAddress, Memory, Offset, OperandSize, Source},
    register::Register,
};
use crate::repr::{LocalIdx, ty::Ty};
use std::collections::{HashMap, HashSet};

type Edges = HashSet<(LocalIdx, LocalIdx)>;

#[derive(Debug, Clone)]
pub enum Location {
    Register(Register),
    Address {
        effective_address: EffectiveAddress,
        spilled: bool,
    },
}

impl Location {
    pub fn to_source(&self, size: OperandSize) -> Source {
        match self {
            Self::Register(r) => r.resize(size).into(),
            Self::Address {
                effective_address, ..
            } => Source::Memory(Memory {
                effective_address: effective_address.clone(),
                size,
            }),
        }
    }

    pub fn to_dest(&self, size: OperandSize) -> Destination {
        match self {
            Self::Register(r) => r.resize(size).into(),
            Self::Address {
                effective_address, ..
            } => Destination::Memory(Memory {
                effective_address: effective_address.clone(),
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

/// A weird looking graph coloring by simplification register allocator
pub struct Allocator {
    nodes: HashMap<LocalIdx, Ty>,
    edges: Edges,
    registers: Vec<Register>,
    locations: HashMap<LocalIdx, Location>,
    pub stack_frame_size: usize,
    spill_mode: bool,
}

impl Allocator {
    pub fn new(types: Vec<Ty>, edges: Edges, registers: Vec<Register>, spill_mode: bool) -> Self {
        Self {
            nodes: types
                .into_iter()
                .enumerate()
                .map(|(i, ty)| (i, ty))
                .collect(),
            edges,
            registers,
            locations: HashMap::new(),
            stack_frame_size: 0,
            spill_mode,
        }
    }

    fn remove_node(&mut self, node: &LocalIdx) -> (Ty, Edges) {
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

    fn add_node(&mut self, node: LocalIdx, ty: Ty, edges: Edges) {
        self.nodes.insert(node, ty);
        self.edges.extend(edges.into_iter());
    }

    fn neighbors(&self, node: &LocalIdx) -> Vec<&LocalIdx> {
        self.edges
            .iter()
            .filter(|(lhs, rhs)| lhs == node || rhs == node)
            .map(|(lhs, rhs)| if lhs == node { rhs } else { lhs })
            .collect()
    }

    fn min(&self) -> Option<LocalIdx> {
        self.nodes
            .iter()
            // ignore precolored nodes
            .filter(|(node, _)| !self.locations.contains_key(node))
            .map(|(node, _)| (*node, self.neighbors(node).len()))
            .min_by(|(_, a), (_, b)| a.cmp(b))
            .map(|(node, _)| node)
    }

    fn max(&self) -> Option<LocalIdx> {
        self.nodes
            .iter()
            // ignore precolored nodes
            .filter(|(node, _)| !self.locations.contains_key(node))
            .map(|(node, _)| (*node, self.neighbors(node).len()))
            .max_by(|(_, a), (_, b)| a.cmp(b))
            .map(|(node, _)| node)
    }

    fn unique_register(&self, neighbors: &[&LocalIdx]) -> Option<Register> {
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
                return Some(*reg);
            }
        }

        None
    }

    pub fn precolor(&mut self, node: LocalIdx, location: Location) {
        self.locations.insert(node, location);
    }

    pub fn add_edge(&mut self, edge: (LocalIdx, LocalIdx)) {
        if !(self.edges.contains(&edge) || self.edges.contains(&(edge.1, edge.0))) {
            self.edges.insert(edge);
        }
    }

    pub fn create_node(&mut self, ty: Ty) -> LocalIdx {
        let node = self.nodes.len();
        self.nodes.insert(node, ty);

        node
    }

    pub fn allocate(mut self) -> (Vec<Location>, usize) {
        let locations = self.locations.clone();
        let nodes = self.nodes.clone();
        let edges = self.edges.clone();
        let mut stack: Vec<(LocalIdx, Ty, Edges)> = Vec::new();
        let mut redo = false;

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
            let ty_size = Abi::ty_size(&ty);
            let force_stack = matches!(ty, Ty::Struct(..));
            self.add_node(node, ty, edges);

            if self.unique_register(&self.neighbors(&node)).is_some() && !force_stack {
                self.locations.insert(
                    node,
                    self.unique_register(&self.neighbors(&node)).unwrap().into(),
                );
            } else {
                if self.spill_mode {
                    self.stack_frame_size += ty_size;
                    self.locations.insert(
                        node,
                        Location::Address {
                            effective_address: EffectiveAddress {
                                base: Base::Register(Register::Rbp),
                                index: None,
                                scale: None,
                                displacement: Some(Offset(-(self.stack_frame_size as isize))),
                            },
                            spilled: true,
                        },
                    );
                } else {
                    self.spill_mode = true;
                    redo = true;
                    break;
                }
            }
        }

        if redo {
            self.locations = locations;
            self.nodes = nodes;
            self.edges = edges;

            self.allocate()
        } else {
            let mut operands: Vec<_> = self.locations.into_iter().collect();
            operands.sort_by(|(a, _), (b, _)| a.cmp(&b));

            (
                operands.into_iter().map(|(_, operand)| operand).collect(),
                self.stack_frame_size,
            )
        }
    }
}

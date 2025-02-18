use super::{operands::Destination, register::Register};
use crate::repr::RegisterId;
use std::collections::{HashMap, HashSet};

/// A weird looking graph coloring by simplification register allocator
pub struct Allocator {
    v_regs: HashSet<RegisterId>,
    interference: HashSet<(RegisterId, RegisterId)>,
    registers: Vec<Register>,
    locations: HashMap<RegisterId, Destination>,
}

impl Allocator {
    pub fn new(
        v_regs_len: usize,
        interference: HashSet<(RegisterId, RegisterId)>,
        registers: Vec<Register>,
    ) -> Self {
        Self {
            v_regs: (0..v_regs_len).into_iter().collect(),
            interference,
            registers,
            locations: HashMap::new(),
        }
    }

    fn remove_vreg(&mut self, vreg: &RegisterId) -> HashSet<(RegisterId, RegisterId)> {
        let edges: HashSet<_> = self
            .interference
            .iter()
            .filter(|(lhs, rhs)| lhs == vreg || rhs == vreg)
            .cloned()
            .collect();

        self.v_regs.remove(&vreg);
        edges
            .iter()
            .for_each(|edge| _ = self.interference.remove(edge));

        edges
    }

    fn add_vreg(&mut self, vreg: RegisterId, edges: HashSet<(RegisterId, RegisterId)>) {
        self.v_regs.insert(vreg);
        self.interference.extend(edges.into_iter());
    }

    fn neighbors(&self, vreg: &RegisterId) -> Vec<&RegisterId> {
        self.interference
            .iter()
            .filter(|(lhs, rhs)| lhs == vreg || rhs == vreg)
            .map(|(lhs, rhs)| if lhs == vreg { rhs } else { lhs })
            .collect()
    }

    fn min(&self) -> Option<RegisterId> {
        self.v_regs
            .iter()
            // ignore precolored nodes
            .filter(|vreg| !self.locations.contains_key(vreg))
            .map(|vreg| (*vreg, self.neighbors(vreg).len()))
            .min_by(|(_, a), (_, b)| a.cmp(b))
            .map(|(vreg, _)| vreg)
    }

    fn max(&self) -> Option<RegisterId> {
        self.v_regs
            .iter()
            // ignore precolored nodes
            .filter(|vreg| !self.locations.contains_key(vreg))
            .map(|vreg| (*vreg, self.neighbors(vreg).len()))
            .max_by(|(_, a), (_, b)| a.cmp(b))
            .map(|(vreg, _)| vreg)
    }

    fn unique_register(&self, neighbors: &[&RegisterId]) -> Register {
        assert!(neighbors.len() < self.registers.len());
        for reg in &self.registers {
            let mut found = true;
            for neighbor in neighbors {
                if let Destination::Register(r) = &self.locations[neighbor] {
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

    pub fn precolor(&mut self, vreg: RegisterId, dest: Destination) {
        self.locations.insert(vreg, dest);
    }

    pub fn add_edge(&mut self, edge: (RegisterId, RegisterId)) {
        if !(self.interference.contains(&edge) || self.interference.contains(&(edge.1, edge.0))) {
            self.interference.insert(edge);
        }
    }

    pub fn create_vreg(&mut self) -> RegisterId {
        let r = self.v_regs.len();
        self.v_regs.insert(r);

        r
    }

    pub fn allocate(mut self) -> Vec<Destination> {
        let mut stack: Vec<(RegisterId, HashSet<(RegisterId, RegisterId)>)> = Vec::new();

        while self.min().is_some() {
            let vreg = if self.min().unwrap() < self.registers.len() {
                self.min()
            } else {
                self.max()
            }
            .unwrap();
            let edges = self.remove_vreg(&vreg);

            stack.push((vreg, edges));
        }

        for (vreg, edges) in stack.into_iter().rev() {
            if edges.len() < self.registers.len() {
                self.add_vreg(vreg, edges);
                // Skip if the vreg was precolored
                self.locations.insert(
                    vreg,
                    Destination::Register(self.unique_register(&self.neighbors(&vreg))),
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

use crate::{
    mir::{self, StackFrameIdx, VregIdx, interference_graph::InterferenceGraph},
    targets::RegisterInfo,
};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub enum Location {
    Register(mir::PhysicalRegister),
    Spill(StackFrameIdx),
}

/// A weird looking graph coloring by simplification register allocator
pub struct Allocator<'a, 'mir, 'hir> {
    register_info: &'a dyn RegisterInfo,
    locations: HashMap<VregIdx, Location>,
    spill_mode: bool,
    function: &'mir mut mir::Function<'hir>,
}

impl<'a, 'mir, 'hir> Allocator<'a, 'mir, 'hir> {
    pub fn new(
        register_info: &'a dyn RegisterInfo,
        precolored: HashMap<VregIdx, Location>,
        spill_mode: bool,
        function: &'mir mut mir::Function<'hir>,
    ) -> Self {
        Self {
            register_info,
            locations: precolored,
            spill_mode,
            function,
        }
    }

    fn unique_register(
        &self,
        node: VregIdx,
        neighbors: &HashSet<VregIdx>,
    ) -> Option<mir::PhysicalRegister> {
        for r in self
            .register_info
            .get_registers_by_class(&self.function.vregs[&node])
        {
            let mut found = true;

            for neighbor in neighbors {
                if let Location::Register(neighbor_r) = self.locations.get(&neighbor).unwrap() {
                    if self.register_info.overlaps(r, neighbor_r) {
                        found = false;

                        break;
                    }
                }
            }

            if found {
                return Some(*r);
            }
        }

        None
    }

    pub fn allocate(mut self) -> HashMap<VregIdx, Location> {
        let locations = self.locations.clone();
        let mut nodes = self
            .function
            .vregs
            .keys()
            .cloned()
            .collect::<Vec<VregIdx>>();
        nodes.sort();
        let mut graph = self.function.interference();
        let mut stack: Vec<(VregIdx, HashSet<VregIdx>)> = Vec::new();
        let mut redo = false;

        while let Some((node, neighbors_count)) = min(&graph, &self.locations, &nodes) {
            let (node, _) = if neighbors_count
                < self
                    .register_info
                    .get_registers_by_class(&self.function.vregs[&node])
                    .len()
            {
                min(&graph, &self.locations, &nodes)
            } else {
                max(&graph, &self.locations, &nodes)
            }
            .unwrap();
            let neighbors = graph.neighbors(&node).clone();
            let pos = nodes.iter().position(|idx| idx == &node).unwrap();

            nodes.remove(pos);
            graph.remove_node(&node);
            stack.push((node, neighbors));
        }

        for (node, neighbors) in stack.into_iter().rev() {
            graph.add_node(node);
            for neighbor in neighbors {
                graph.add_edge(node, neighbor);
            }

            if let Some(r) = self.unique_register(node, graph.neighbors(&node)) {
                self.locations.insert(node, Location::Register(r));
            } else {
                if self.spill_mode {
                    let idx = self.function.next_stack_frame_idx;
                    self.function.next_stack_frame_idx += 1;

                    self.locations.insert(node, Location::Spill(idx));
                } else {
                    self.spill_mode = true;
                    redo = true;
                    break;
                }
            }
        }

        if redo {
            self.locations = locations;

            self.allocate()
        } else {
            self.function
                .vregs
                .iter()
                .map(|(vreg, _)| (*vreg, self.locations[vreg].clone()))
                .collect()
        }
    }
}

fn min<T: Copy + Eq + std::hash::Hash>(
    graph: &InterferenceGraph<T>,
    locations: &HashMap<T, Location>,
    nodes: &[T],
) -> Option<(T, usize)> {
    nodes
        .iter()
        // ignore precolored nodes
        .filter(|node| !locations.contains_key(node))
        .map(|node| (*node, graph.neighbors(node).len()))
        .min_by_key(|(_, len)| *len)
}

fn max<T: Copy + Eq + std::hash::Hash>(
    graph: &InterferenceGraph<T>,
    locations: &HashMap<T, Location>,
    nodes: &[T],
) -> Option<(T, usize)> {
    nodes
        .iter()
        // ignore precolored nodes
        .filter(|node| !locations.contains_key(node))
        .map(|node| (*node, graph.neighbors(node).len()))
        .max_by_key(|(_, len)| *len)
}

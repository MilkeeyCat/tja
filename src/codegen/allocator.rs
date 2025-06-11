use crate::{
    mir::{
        self, Register, StackFrameIdx, VregIdx,
        interference_graph::{InterferenceGraph, NodeId},
    },
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
        spill_mode: bool,
        function: &'mir mut mir::Function<'hir>,
    ) -> Self {
        Self {
            register_info,
            locations: HashMap::new(),
            spill_mode,
            function,
        }
    }

    fn unique_register(
        &self,
        graph: &InterferenceGraph,
        node_id: NodeId,
    ) -> Option<mir::PhysicalRegister> {
        let vreg_idx = match graph.get_node(node_id) {
            Register::Virtual(idx) => idx,
            Register::Physical(_) => unreachable!(),
        };

        for r in self
            .register_info
            .get_registers_by_class(&self.function.vreg_classes[&vreg_idx])
        {
            let mut found = true;

            for neighbor in graph.neighbors(node_id) {
                match graph.get_node(*neighbor) {
                    Register::Virtual(idx) => {
                        if let Location::Register(neighbor_r) = &self.locations[idx] {
                            if self.register_info.overlaps(r, neighbor_r) {
                                found = false;

                                break;
                            }
                        }
                    }
                    Register::Physical(neighbor_r) => {
                        if self.register_info.overlaps(r, neighbor_r) {
                            found = false;

                            break;
                        }
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
        let (mut graph, mut node_ids) = self.function.interference();
        let mut stack: Vec<(VregIdx, NodeId, HashSet<NodeId>)> = Vec::new();
        let mut redo = false;

        while let Some(node_id) = min(&graph, &node_ids) {
            let neighbors_count = graph.neighbors(node_id).len();
            let vreg_idx = match graph.get_node(node_id) {
                Register::Virtual(idx) => *idx,
                Register::Physical(_) => unreachable!(),
            };
            let node_id = if neighbors_count
                < self
                    .register_info
                    .get_registers_by_class(&self.function.vreg_classes[&vreg_idx])
                    .len()
            {
                min(&graph, &node_ids)
            } else {
                max(&graph, &node_ids)
            }
            .unwrap();
            let neighbors = graph.neighbors(node_id).clone();
            let pos = node_ids.iter().position(|idx| idx == &node_id).unwrap();

            node_ids.remove(pos);
            graph.remove_node(node_id);
            stack.push((vreg_idx, node_id, neighbors));
        }

        for (vreg_idx, node_id, neighbors) in stack.into_iter().rev() {
            graph.add_node_with_node_id(Register::Virtual(vreg_idx), node_id);

            for neighbor in neighbors {
                graph.add_edge(node_id, neighbor);
            }

            if let Some(r) = self.unique_register(&graph, node_id) {
                self.locations.insert(vreg_idx, Location::Register(r));
            } else {
                if self.spill_mode {
                    let idx = self.function.next_stack_frame_idx;
                    self.function.next_stack_frame_idx += 1;

                    self.locations.insert(vreg_idx, Location::Spill(idx));
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
            self.locations
        }
    }
}

fn min(graph: &InterferenceGraph, node_ids: &[NodeId]) -> Option<NodeId> {
    node_ids
        .iter()
        .min_by_key(|node_id| graph.neighbors(**node_id).len())
        .cloned()
}

fn max(graph: &InterferenceGraph, nodes_ids: &[NodeId]) -> Option<NodeId> {
    nodes_ids
        .iter()
        .max_by_key(|node_id| graph.neighbors(**node_id).len())
        .cloned()
}

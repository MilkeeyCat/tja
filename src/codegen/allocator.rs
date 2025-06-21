use crate::{
    mir::{
        self, Operand, Register, RegisterRole, StackFrameIdx, VregIdx,
        function::Value,
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
struct Allocator<'a, 'mir, 'hir, RI: RegisterInfo> {
    register_info: &'a RI,
    locations: HashMap<(VregIdx, usize), Location>,
    spill_mode: bool,
    function: &'mir mut mir::Function<'hir>,
}

impl<'a, 'mir, 'hir, RI: RegisterInfo> Allocator<'a, 'mir, 'hir, RI> {
    fn new(
        register_info: &'a RI,
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
            Value::Virtual(idx, _) => idx,
            Value::Physical(_) => unreachable!(),
        };

        for r in self
            .register_info
            .get_registers_by_class(&self.function.vreg_classes[&vreg_idx])
        {
            let mut found = true;

            for neighbor in graph.neighbors(node_id) {
                match graph.get_node(*neighbor) {
                    Value::Virtual(idx, def_idx) => {
                        if let Location::Register(neighbor_r) = &self.locations[&(*idx, *def_idx)] {
                            if self.register_info.overlaps(r, neighbor_r) {
                                found = false;

                                break;
                            }
                        }
                    }
                    Value::Physical(neighbor_r) => {
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

pub fn allocate<RI: RegisterInfo>(
    register_info: &RI,
    spill_mode: bool,
    function: &mut mir::Function,
) {
    let mut allocator = Allocator::new(register_info, spill_mode, function);
    let locations = allocator.locations.clone();
    let (mut graph, mut node_ids) = allocator.function.interference();
    let mut stack: Vec<((VregIdx, usize), NodeId, HashSet<NodeId>)> = Vec::new();
    let mut redo = false;

    while let Some(node_id) = min(&graph, &node_ids) {
        let neighbors_count = graph.neighbors(node_id).len();
        let vreg_value = match graph.get_node(node_id) {
            Value::Virtual(idx, def_idx) => (*idx, *def_idx),
            Value::Physical(_) => unreachable!(),
        };
        let node_id = if neighbors_count
            < allocator
                .register_info
                .get_registers_by_class(&allocator.function.vreg_classes[&vreg_value.0])
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
        stack.push((vreg_value, node_id, neighbors));
    }

    for ((vreg_idx, def_idx), node_id, neighbors) in stack.into_iter().rev() {
        graph.add_node_with_node_id(Value::Virtual(vreg_idx, def_idx), node_id);

        for neighbor in neighbors {
            graph.add_edge(node_id, neighbor);
        }

        if let Some(r) = allocator.unique_register(&graph, node_id) {
            allocator
                .locations
                .insert((vreg_idx, def_idx), Location::Register(r));
        } else {
            if allocator.spill_mode {
                let idx = allocator.function.next_stack_frame_idx;
                allocator.function.next_stack_frame_idx += 1;

                allocator
                    .locations
                    .insert((vreg_idx, def_idx), Location::Spill(idx));
            } else {
                allocator.spill_mode = true;
                redo = true;
                break;
            }
        }
    }

    if redo {
        allocator.locations = locations;

        allocate(register_info, allocator.spill_mode, function)
    } else {
        let locations = allocator.locations;
        let mut vregs_occurences: HashMap<VregIdx, usize> = HashMap::new();

        for bb in &mut function.blocks {
            for instr in &mut bb.instructions {
                for operand in &mut instr.operands {
                    if let Operand::Register(Register::Virtual(idx), role) = operand {
                        let def_idx = vregs_occurences
                            .entry(*idx)
                            .and_modify(|def_idx| {
                                if let RegisterRole::Def = role {
                                    *def_idx += 1;
                                }
                            })
                            .or_default();

                        match locations[&(*idx, *def_idx)] {
                            Location::Register(r) => {
                                *operand = Operand::Register(Register::Physical(r), role.clone());
                            }
                            Location::Spill(_stack_frame_idx) => unimplemented!(),
                        };
                    }
                }
            }
        }
    }
}

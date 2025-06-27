use crate::{
    hir::ty,
    mir::{
        self, BasicBlockPatch, Operand, Register, RegisterRole, StackFrameIdx, VregIdx,
        function::Value,
        interference_graph::{InterferenceGraph, NodeId},
    },
    targets::{Abi, RegisterInfo, Target},
};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub enum Location {
    Register(mir::PhysicalRegister),
    Spill(StackFrameIdx),
}

/// A weird looking graph coloring by simplification register allocator
struct Allocator<'a, 'hir, T: Target> {
    target: &'a T,
    locations: HashMap<(VregIdx, usize), Location>,
    spill_mode: bool,
    function: &'a mut mir::Function<'hir>,
}

impl<'a, 'hir, T: Target> Allocator<'a, 'hir, T> {
    fn new(target: &'a T, spill_mode: bool, function: &'a mut mir::Function<'hir>) -> Self {
        Self {
            target,
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
            .target
            .register_info()
            .get_registers_by_class(&self.function.vreg_classes[&vreg_idx])
            .iter()
            .skip(self.spill_mode as usize)
        {
            let mut found = true;

            for neighbor in graph.neighbors(node_id) {
                match graph.get_node(*neighbor) {
                    Value::Virtual(idx, def_idx) => {
                        if let Location::Register(neighbor_r) = &self.locations[&(*idx, *def_idx)] {
                            if self.target.register_info().overlaps(r, neighbor_r) {
                                found = false;

                                break;
                            }
                        }
                    }
                    Value::Physical(neighbor_r) => {
                        if self.target.register_info().overlaps(r, neighbor_r) {
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

pub fn allocate<T: Target>(
    target: &T,
    spill_mode: bool,
    function: &mut mir::Function,
    ty_storage: &ty::Storage,
) {
    let mut allocator = Allocator::new(target, spill_mode, function);
    let (mut graph, mut node_ids) = allocator.function.interference();
    let mut stack: Vec<((VregIdx, usize), NodeId, HashSet<NodeId>)> = Vec::new();
    let mut redo = false;

    while let Some(node_id) = min(&graph, &node_ids) {
        let neighbors_count = graph.neighbors(node_id).len();
        let vreg_idx = match graph.get_node(node_id) {
            Value::Virtual(idx, _) => idx,
            Value::Physical(_) => unreachable!(),
        };
        let node_id = if neighbors_count
            < allocator
                .target
                .register_info()
                .get_registers_by_class(&allocator.function.vreg_classes[vreg_idx])
                .len()
        {
            min(&graph, &node_ids)
        } else {
            max(&graph, &node_ids)
        }
        .unwrap();
        let vreg_value = match graph.get_node(node_id) {
            Value::Virtual(idx, def_idx) => (*idx, *def_idx),
            Value::Physical(_) => unreachable!(),
        };

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
                allocator.function.stack_slots.insert(
                    idx,
                    allocator
                        .target
                        .abi()
                        .ty_size(ty_storage, allocator.function.vreg_types[&vreg_idx]),
                );

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
        allocate(target, allocator.spill_mode, function, ty_storage)
    } else {
        let locations = allocator.locations;
        let mut vregs_occurences: HashMap<VregIdx, usize> = HashMap::new();

        for bb in &mut function.blocks {
            let mut patch = BasicBlockPatch::new();

            for (instr_idx, instr) in bb.instructions.iter_mut().enumerate() {
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
                            Location::Spill(frame_idx) => {
                                let r = target
                                    .register_info()
                                    .get_registers_by_class(&function.vreg_classes[idx])[0];
                                let size =
                                    target.abi().ty_size(ty_storage, function.vreg_types[idx]);

                                target.load_reg_from_stack_slot(
                                    &mut patch, instr_idx, r, frame_idx, size,
                                );
                                *operand = Operand::Register(Register::Physical(r), role.clone());
                                target.store_reg_to_stack_slot(
                                    &mut patch,
                                    instr_idx + 1,
                                    r,
                                    frame_idx,
                                    size,
                                );
                            }
                        };
                    }
                }
            }

            patch.apply(bb);
        }
    }
}

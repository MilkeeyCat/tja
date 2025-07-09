use crate::{
    mir::{
        self, BasicBlockPatch, Function, Operand, Register, StackFrameIdx, VregIdx,
        interference_graph::{InterferenceGraph, NodeId},
    },
    pass::{Context, Pass},
    targets::{Abi, RegisterInfo, Target},
};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub enum Location {
    Register(mir::PhysicalRegister),
    Spill(StackFrameIdx),
}

/// A weird looking graph coloring by simplification register allocator
#[derive(Default)]
pub struct Allocator {
    spill_mode: bool,
}

impl Allocator {
    pub fn new(spill_mode: bool) -> Self {
        Self { spill_mode }
    }

    fn unique_register<T: Target>(
        &self,
        target: &T,
        func: &Function,
        graph: &InterferenceGraph,
        node_id: NodeId,
        locations: &HashMap<VregIdx, Location>,
    ) -> Option<mir::PhysicalRegister> {
        let vreg_idx = match graph.get_node(node_id) {
            Register::Virtual(idx) => idx,
            Register::Physical(_) => unreachable!(),
        };

        for r in target
            .register_info()
            .get_registers_by_class(&func.vreg_classes[&vreg_idx])
            .iter()
            .skip(self.spill_mode as usize)
        {
            let mut found = true;

            for neighbor in graph.neighbors(node_id) {
                match graph.get_node(*neighbor) {
                    Register::Virtual(idx) => {
                        if let Location::Register(neighbor_r) = &locations[idx] {
                            if target.register_info().overlaps(r, neighbor_r) {
                                found = false;

                                break;
                            }
                        }
                    }
                    Register::Physical(neighbor_r) => {
                        if target.register_info().overlaps(r, neighbor_r) {
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

impl<'a, T: Target> Pass<'a, Function, T> for Allocator {
    fn run(&self, func: &mut Function, ctx: &mut Context<'a, T>) {
        let (mut graph, mut node_ids) = func.interference();
        let mut stack: Vec<(VregIdx, NodeId, HashSet<NodeId>)> = Vec::new();
        let mut locations: HashMap<VregIdx, Location> = HashMap::new();
        let mut redo = false;

        while let Some(node_id) = min(&graph, &node_ids) {
            let neighbors_count = graph.neighbors(node_id).len();
            let vreg_idx = match graph.get_node(node_id) {
                Register::Virtual(idx) => idx,
                Register::Physical(_) => unreachable!(),
            };
            let node_id = if neighbors_count
                < ctx
                    .target
                    .register_info()
                    .get_registers_by_class(&func.vreg_classes[vreg_idx])
                    .len()
            {
                min(&graph, &node_ids)
            } else {
                max(&graph, &node_ids)
            }
            .unwrap();
            let vreg_idx = match graph.get_node(node_id) {
                Register::Virtual(idx) => *idx,
                Register::Physical(_) => unreachable!(),
            };

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

            if let Some(r) = self.unique_register(ctx.target, func, &graph, node_id, &locations) {
                locations.insert(vreg_idx, Location::Register(r));
            } else {
                if self.spill_mode {
                    let idx = func.next_stack_frame_idx;
                    func.next_stack_frame_idx += 1;
                    func.stack_slots.insert(
                        idx,
                        ctx.target
                            .abi()
                            .ty_size(ctx.ty_storage, func.vreg_types[&vreg_idx]),
                    );

                    locations.insert(vreg_idx, Location::Spill(idx));
                } else {
                    redo = true;
                    break;
                }
            }
        }

        if redo {
            Self::new(true).run(func, ctx)
        } else {
            for bb in &mut func.blocks {
                let mut patch = BasicBlockPatch::new();

                for (instr_idx, instr) in bb.instructions.iter_mut().enumerate() {
                    for operand in &mut instr.operands {
                        if let Operand::Register(Register::Virtual(idx), role) = operand {
                            match locations[idx] {
                                Location::Register(r) => {
                                    *operand =
                                        Operand::Register(Register::Physical(r), role.clone());
                                }
                                Location::Spill(frame_idx) => {
                                    let r = ctx
                                        .target
                                        .register_info()
                                        .get_registers_by_class(&func.vreg_classes[idx])[0];
                                    let size = ctx
                                        .target
                                        .abi()
                                        .ty_size(ctx.ty_storage, func.vreg_types[idx]);

                                    ctx.target.load_reg_from_stack_slot(
                                        &mut patch, instr_idx, r, frame_idx, size,
                                    );
                                    *operand =
                                        Operand::Register(Register::Physical(r), role.clone());
                                    ctx.target.store_reg_to_stack_slot(
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

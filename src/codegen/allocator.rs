use crate::{
    mir::{
        BasicBlockPatch, BlockIdx, Function, InstructionIdx, Operand, PhysicalRegister, Register,
        RegisterRole, StackFrameIdx, VregIdx,
    },
    pass::{Context, Pass},
    targets::{Abi, RegisterInfo, Target},
};
use std::collections::{HashMap, HashSet};

pub trait RemoveByValue<V> {
    fn remove_by_val(&mut self, value: &V) -> bool;
}

impl<V: PartialEq> RemoveByValue<V> for Vec<V> {
    fn remove_by_val(&mut self, value: &V) -> bool {
        match self.iter().position(|v| v == value) {
            Some(idx) => {
                self.remove(idx);

                true
            }
            None => false,
        }
    }
}

#[derive(Default)]
pub struct Allocator;

impl<'a, T: Target> Pass<'a, Function, T> for Allocator {
    fn run(&self, func: &mut Function, ctx: &mut Context<'a, T>) {
        let mut allocator = AllocatorImpl::new(func, ctx, HashSet::new());

        loop {
            allocator.build();
            allocator.make_worklist();

            loop {
                if !allocator.simplify_worklist.is_empty() {
                    allocator.simplify();
                } else if !allocator.worklist_moves.is_empty() {
                    allocator.coalesce();
                } else if !allocator.freeze_worklist.is_empty() {
                    allocator.freeze();
                } else if !allocator.spill_worklist.is_empty() {
                    allocator.select_spill();
                } else {
                    break;
                }
            }

            allocator.assign_colors();

            if !allocator.spilled_nodes.is_empty() {
                let spilled_nodes = std::mem::take(&mut allocator.spilled_nodes);
                let spills: HashMap<VregIdx, StackFrameIdx> = spilled_nodes
                    .iter()
                    .map(|vreg_idx| {
                        let idx = func.next_stack_frame_idx;

                        func.next_stack_frame_idx += 1;
                        func.stack_slots.insert(
                            idx,
                            ctx.target
                                .abi()
                                .ty_size(ctx.ty_storage, func.vreg_types[&vreg_idx]),
                        );

                        (*vreg_idx, idx)
                    })
                    .collect();

                for bb in &mut func.blocks {
                    let mut patch = BasicBlockPatch::new();

                    for (instr_idx, instr) in bb.instructions.iter_mut().enumerate() {
                        for operand in &mut instr.operands {
                            if let Operand::Register(Register::Virtual(idx), role) = operand
                                && spills.contains_key(idx)
                            {
                                let frame_idx = spills[idx];
                                let ty = func.vreg_types[idx];
                                let size = ctx.target.abi().ty_size(ctx.ty_storage, ty);
                                let vreg_idx = func.next_vreg_idx;

                                func.next_vreg_idx += 1;
                                func.vreg_types.insert(vreg_idx, ty);
                                func.vreg_classes.insert(vreg_idx, func.vreg_classes[idx]);

                                *idx = vreg_idx;

                                match role {
                                    RegisterRole::Def => {
                                        ctx.target.store_reg_to_stack_slot(
                                            &mut patch,
                                            instr_idx + 1,
                                            vreg_idx,
                                            frame_idx,
                                            size,
                                        );
                                    }
                                    RegisterRole::Use => {
                                        ctx.target.load_reg_from_stack_slot(
                                            &mut patch, instr_idx, vreg_idx, frame_idx, size,
                                        );
                                    }
                                }
                            }
                        }
                    }

                    patch.apply(bb);
                }

                for vreg_idx in spills.keys() {
                    func.vreg_types.remove(vreg_idx);
                    func.vreg_classes.remove(vreg_idx);
                }

                allocator = AllocatorImpl::new(func, ctx, spilled_nodes.into_iter().collect());
            } else {
                let colors = std::mem::take(&mut allocator.color);
                let mut patches = HashMap::new();

                for (bb_idx, instr_idx) in &allocator.coalesced_moves {
                    patches
                        .entry(*bb_idx)
                        .or_insert_with(|| BasicBlockPatch::new())
                        .delete_instruction(*instr_idx)
                }

                for (idx, patch) in patches {
                    patch.apply(&mut func.blocks[idx]);
                }

                for bb in &mut func.blocks {
                    for instr in bb.instructions.iter_mut() {
                        for operand in &mut instr.operands {
                            if let Operand::Register(Register::Virtual(idx), role) = operand {
                                *operand = Operand::Register(
                                    Register::Physical(colors[idx]),
                                    role.clone(),
                                );
                            }
                        }
                    }
                }

                break;
            }
        }
    }
}

// When in doubt refer to https://c9x.me/compile/bib/irc.pdf
pub struct AllocatorImpl<'a, 'b, T: Target> {
    function: &'a Function,
    ctx: &'a Context<'b, T>,
    /// List of nodes that are generated by spill code
    spill_temps: HashSet<VregIdx>,

    /// Low-degree non-move-related nodes
    simplify_worklist: Vec<VregIdx>,
    // Low-degree move-related nodes
    freeze_worklist: Vec<VregIdx>,
    /// High-degree nodes
    spill_worklist: Vec<VregIdx>,
    /// Nodes marked for spilling during this round
    spilled_nodes: Vec<VregIdx>,
    /// Registers that have been coalesced, when the move u := v is coalesced,
    /// one of u or v is added to this set, and the other is put back on some
    /// worklist
    coalesced_nodes: HashSet<VregIdx>,
    /// Nodes successfully colored
    colored_nodes: HashSet<VregIdx>,
    /// Stack containing temporaries removed from the graph
    select_stack: Vec<VregIdx>,

    /// Moves that have been coalesced
    coalesced_moves: HashSet<(BlockIdx, InstructionIdx)>,
    /// Moves whose source and target interfere
    constrained_moves: HashSet<(BlockIdx, InstructionIdx)>,
    /// Moves that will no longer be considered for coalescing
    frozen_moves: HashSet<(BlockIdx, InstructionIdx)>,
    /// Moves enabled for possible coalescing
    worklist_moves: Vec<(BlockIdx, InstructionIdx)>,
    /// Moves not yet ready for coalescing
    active_moves: HashSet<(BlockIdx, InstructionIdx)>,

    /// The set of interference edges (u, v) in the graph. If (u, v) ∈ adj_set
    /// then (v, u) ∈ adj_set
    adj_set: HashSet<(Register, Register)>,
    /// Adjacency list representation of the graph, for each nonprecolored
    /// temporary u, adj_list[u] is the set of nodes that interfere with u
    adj_list: HashMap<VregIdx, HashSet<Register>>,
    /// The current degree of each node
    degree: HashMap<VregIdx, usize>,

    /// A mapping from node to the list of moves it is associated with
    move_list: HashMap<Register, HashSet<(BlockIdx, InstructionIdx)>>,
    /// When a move (u, v) has been coalesced, and v put in coalesced_nodes, then
    /// alias(v) = u
    alias: HashMap<VregIdx, Register>,
    /// The color chosen by the algorithm for a node
    color: HashMap<VregIdx, PhysicalRegister>,
}

impl<'a, 'b, T: Target> AllocatorImpl<'a, 'b, T> {
    fn new(func: &'a Function, ctx: &'a mut Context<'b, T>, spill_temps: HashSet<VregIdx>) -> Self {
        let mut degree = HashMap::new();
        let mut adj_list = HashMap::new();

        for r in func.registers() {
            if let Register::Virtual(vreg_idx) = r {
                degree.insert(vreg_idx, 0);
                adj_list.insert(vreg_idx, Default::default());
            }
        }

        Self {
            function: func,
            ctx,
            spill_temps,

            simplify_worklist: Vec::new(),
            freeze_worklist: Vec::new(),
            spill_worklist: Vec::new(),
            spilled_nodes: Vec::new(),
            select_stack: Vec::new(),
            coalesced_nodes: HashSet::new(),
            colored_nodes: HashSet::new(),

            coalesced_moves: HashSet::new(),
            constrained_moves: HashSet::new(),
            frozen_moves: HashSet::new(),
            worklist_moves: Vec::new(),
            active_moves: HashSet::new(),

            adj_set: HashSet::new(),
            adj_list,
            degree,

            move_list: HashMap::new(),
            alias: HashMap::new(),
            color: HashMap::new(),
        }
    }

    fn add_edge(&mut self, a: Register, b: Register) {
        if !self.adj_set.contains(&(a.clone(), b.clone())) {
            assert!(a != b);

            self.adj_set.insert((a.clone(), b.clone()));
            self.adj_set.insert((b.clone(), a.clone()));

            if let Register::Virtual(idx) = a.clone() {
                self.adj_list.get_mut(&idx).unwrap().insert(b.clone());
                *self.degree.get_mut(&idx).unwrap() += 1;
            }

            if let Register::Virtual(idx) = b {
                self.adj_list.get_mut(&idx).unwrap().insert(a);
                *self.degree.get_mut(&idx).unwrap() += 1;
            }
        }
    }

    fn build(&mut self) {
        for ((bb_idx, bb), liveness) in self
            .function
            .blocks
            .iter()
            .enumerate()
            .zip(self.function.liveness())
        {
            let mut live = liveness.outs;

            for (instr_idx, instr) in bb.instructions.iter().enumerate().rev() {
                let defs_uses = instr.defs_uses();

                if self.ctx.target.is_move_op(instr.opcode) {
                    live = &live - &defs_uses.uses;

                    for r in defs_uses.defs.union(&defs_uses.uses) {
                        self.move_list
                            .entry(r.clone())
                            .or_default()
                            .insert((bb_idx, instr_idx));
                    }

                    self.worklist_moves.push((bb_idx, instr_idx));
                }

                for def in &defs_uses.defs {
                    for out in &live {
                        if def != out {
                            self.add_edge(def.clone(), out.clone());
                        }
                    }
                }

                live = defs_uses
                    .uses
                    .union(&(&live - &defs_uses.defs))
                    .cloned()
                    .collect();
            }
        }
    }

    fn node_moves(&self, r: &Register) -> HashSet<(BlockIdx, InstructionIdx)> {
        self.move_list
            .get(r)
            .cloned()
            .unwrap_or_default()
            .intersection(
                &self
                    .active_moves
                    .union(&self.worklist_moves.clone().into_iter().collect())
                    .cloned()
                    .collect(),
            )
            .cloned()
            .collect()
    }

    fn move_related(&self, r: &Register) -> bool {
        !self.node_moves(r).is_empty()
    }

    fn make_worklist(&mut self) {
        for r in self.function.registers() {
            if let Register::Virtual(idx) = r {
                if self.degree[&idx]
                    >= self
                        .ctx
                        .target
                        .register_info()
                        .get_registers_by_class(&self.function.vreg_classes[&idx])
                        .len()
                {
                    self.spill_worklist.push(idx);
                } else if self.move_related(&r) {
                    self.freeze_worklist.push(idx);
                } else {
                    self.simplify_worklist.push(idx);
                }
            }
        }
    }

    fn adjacent(&self, vreg_idx: &VregIdx) -> HashSet<Register> {
        &self.adj_list[vreg_idx]
            - &self
                .select_stack
                .iter()
                .map(|vreg_idx| Register::Virtual(*vreg_idx))
                .collect::<HashSet<_>>()
                .union(
                    &self
                        .coalesced_nodes
                        .iter()
                        .map(|vreg_idx| Register::Virtual(*vreg_idx))
                        .collect(),
                )
                .cloned()
                .collect()
    }

    fn enable_moves(&mut self, nodes: HashSet<Register>) {
        for node in nodes {
            for mv in self.node_moves(&node) {
                if self.active_moves.contains(&mv) {
                    self.active_moves.remove(&mv);
                    self.worklist_moves.push(mv);
                }
            }
        }
    }

    fn decrement_degree(&mut self, vreg_idx: &VregIdx) {
        let degree = self.degree[vreg_idx];

        *self.degree.get_mut(vreg_idx).unwrap() -= 1;

        if degree
            == self
                .ctx
                .target
                .register_info()
                .get_registers_by_class(&self.function.vreg_classes[&vreg_idx])
                .len()
        {
            self.enable_moves(
                HashSet::from([Register::Virtual(*vreg_idx)])
                    .union(&self.adjacent(vreg_idx))
                    .cloned()
                    .collect(),
            );

            self.spill_worklist.remove_by_val(vreg_idx);

            if self.move_related(&Register::Virtual(*vreg_idx)) {
                self.freeze_worklist.push(*vreg_idx);
            } else {
                self.simplify_worklist.push(*vreg_idx);
            }
        }
    }

    fn simplify(&mut self) {
        let vreg_idx = self.simplify_worklist.pop().unwrap();

        self.select_stack.push(vreg_idx);

        for r in self.adjacent(&vreg_idx) {
            if let Register::Virtual(idx) = r {
                self.decrement_degree(&idx);
            }
        }
    }

    fn get_alias(&self, r: &Register) -> Register {
        if let Register::Virtual(idx) = r
            && self.coalesced_nodes.contains(idx)
        {
            self.get_alias(&self.alias[&idx])
        } else {
            r.clone()
        }
    }

    fn add_work_list(&mut self, r: &Register) {
        if let Register::Virtual(idx) = r {
            if !self.move_related(r)
                && self.degree[idx]
                    < self
                        .ctx
                        .target
                        .register_info()
                        .get_registers_by_class(&self.function.vreg_classes[idx])
                        .len()
            {
                self.freeze_worklist.remove_by_val(idx);
                self.simplify_worklist.push(*idx);
            }
        }
    }

    fn george_strat(&self, nodes: HashSet<Register>, u: Register) -> bool {
        nodes.into_iter().all(|node| {
            (match node {
                Register::Virtual(idx) => {
                    self.degree[&idx]
                        < self
                            .ctx
                            .target
                            .register_info()
                            .get_registers_by_class(&self.function.vreg_classes[&idx])
                            .len()
                }
                Register::Physical(_) => true,
            }) || self.adj_set.contains(&(node, u.clone()))
        })
    }

    fn briggs_strat(&self, nodes: HashSet<Register>, vreg_idx: VregIdx) -> bool {
        nodes
            .into_iter()
            .map(|r| match r {
                Register::Virtual(idx)
                    if self.degree[&idx]
                        >= self
                            .ctx
                            .target
                            .register_info()
                            .get_registers_by_class(&self.function.vreg_classes[&idx])
                            .len() =>
                {
                    1
                }
                _ => 0,
            })
            .sum::<usize>()
            < self
                .ctx
                .target
                .register_info()
                .get_registers_by_class(&self.function.vreg_classes[&vreg_idx])
                .len()
    }

    fn combine(&mut self, u: &Register, v: &VregIdx) {
        if self.freeze_worklist.contains(v) {
            self.freeze_worklist.remove_by_val(v);
        } else {
            self.spill_worklist.remove_by_val(v);
        }

        self.coalesced_nodes.insert(*v);
        self.alias.insert(*v, u.clone());

        let v_moves = self.move_list[&Register::Virtual(*v)].clone();
        self.move_list.get_mut(u).unwrap().extend(v_moves);

        for t in self.adjacent(v) {
            self.add_edge(t.clone(), u.clone());

            if let Register::Virtual(idx) = t {
                self.decrement_degree(&idx);
            }
        }

        if let Register::Virtual(idx) = u
            && self.degree[idx]
                >= self
                    .ctx
                    .target
                    .register_info()
                    .get_registers_by_class(&self.function.vreg_classes[&idx])
                    .len()
            && self.freeze_worklist.contains(idx)
        {
            self.freeze_worklist.remove_by_val(idx);
            self.spill_worklist.push(*idx);
        }
    }

    fn coalesce(&mut self) {
        let (bb_idx, instr_idx) = self.worklist_moves.pop().unwrap();

        let mv = &self.function.blocks[bb_idx].instructions[instr_idx];
        let (x, y) = {
            let defs_uses = mv.defs_uses();

            assert_eq!(defs_uses.defs.len(), 1);
            assert_eq!(defs_uses.uses.len(), 1);

            (
                defs_uses.defs.into_iter().next().unwrap(),
                defs_uses.uses.into_iter().next().unwrap(),
            )
        };
        let x = self.get_alias(&x);
        let y = self.get_alias(&y);
        let (u, v) = if matches!(y, Register::Physical(_)) {
            (y, x)
        } else {
            (x, y)
        };

        if u == v {
            self.coalesced_moves.insert((bb_idx, instr_idx));
            self.add_work_list(&u);
        } else if matches!(v, Register::Physical(_))
            || self.adj_set.contains(&(u.clone(), v.clone()))
        {
            self.constrained_moves.insert((bb_idx, instr_idx));
            self.add_work_list(&u);
            self.add_work_list(&v);
        } else if matches!(v, Register::Physical(_))
            && self.george_strat(
                self.adjacent(match &v {
                    Register::Virtual(idx) => idx,
                    _ => unreachable!(),
                }),
                u.clone(),
            )
            || !matches!(v, Register::Physical(_))
                && self.briggs_strat(
                    self.adjacent(match &u {
                        Register::Virtual(idx) => idx,
                        _ => unreachable!(),
                    })
                    .union(&self.adjacent(match &v {
                        Register::Virtual(idx) => idx,
                        _ => unreachable!(),
                    }))
                    .cloned()
                    .collect(),
                    match &u {
                        Register::Virtual(idx) => *idx,
                        _ => unreachable!(),
                    },
                )
        {
            let v = match &v {
                Register::Virtual(idx) => idx,
                Register::Physical(_) => unreachable!(),
            };
            self.coalesced_moves.insert((bb_idx, instr_idx));
            self.combine(&u, v);
            self.add_work_list(&u);
        } else {
            self.active_moves.insert((bb_idx, instr_idx));
        }
    }

    fn freeze_moves(&mut self, vreg_idx: &VregIdx) {
        for mv in self.node_moves(&Register::Virtual(*vreg_idx)) {
            if self.active_moves.contains(&mv) {
                self.active_moves.remove(&mv);
            } else {
                self.worklist_moves.remove_by_val(&mv);
            }

            self.frozen_moves.insert(mv);

            let defs_uses = self.function.blocks[mv.0].instructions[mv.1].defs_uses();
            let r = if defs_uses.defs.iter().next().unwrap() == &Register::Virtual(*vreg_idx) {
                defs_uses.uses.iter().next().unwrap()
            } else {
                defs_uses.defs.iter().next().unwrap()
            };

            if let Register::Virtual(idx) = r
                && self.node_moves(r).is_empty()
                && self.degree[idx]
                    < self
                        .ctx
                        .target
                        .register_info()
                        .get_registers_by_class(&self.function.vreg_classes[idx])
                        .len()
            {
                self.freeze_worklist.remove_by_val(idx);
                self.simplify_worklist.push(*idx);
            }
        }
    }

    fn freeze(&mut self) {
        let vreg_idx = self.freeze_worklist.pop().unwrap();
        self.simplify_worklist.push(vreg_idx);

        self.freeze_moves(&vreg_idx);
    }

    fn select_spill(&mut self) {
        let idx = self
            .spill_worklist
            .iter()
            .position(|vreg_idx| !self.spill_temps.contains(vreg_idx))
            .unwrap();
        let vreg_idx = self.spill_worklist.remove(idx);

        self.simplify_worklist.push(vreg_idx);
        self.freeze_moves(&vreg_idx);
    }

    fn assign_colors(&mut self) {
        for vreg_idx in self.select_stack.iter().rev() {
            let mut colors = self
                .ctx
                .target
                .register_info()
                .get_registers_by_class(&self.function.vreg_classes[&vreg_idx])
                .to_vec();

            for r in &self.adj_list[&vreg_idx] {
                let r = match self.get_alias(r) {
                    Register::Virtual(vreg_idx) => {
                        if self.colored_nodes.contains(&vreg_idx) {
                            Some(self.color[&vreg_idx])
                        } else {
                            None
                        }
                    }
                    Register::Physical(r) => Some(r),
                };

                if let Some(v) = r {
                    colors.retain(|u| !self.ctx.target.register_info().overlaps(u, &v));
                }
            }

            if let Some(r) = colors.into_iter().next() {
                self.colored_nodes.insert(*vreg_idx);
                self.color.insert(*vreg_idx, r);
            } else {
                self.spilled_nodes.push(*vreg_idx);
            }
        }

        for vreg_idx in &self.coalesced_nodes {
            let r = match self.get_alias(&Register::Virtual(*vreg_idx)) {
                Register::Virtual(vreg_idx) => self.color[&vreg_idx],
                Register::Physical(r) => r,
            };

            self.color.insert(*vreg_idx, r);
        }
    }
}

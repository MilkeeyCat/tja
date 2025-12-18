use crate::{
    ConditionCode,
    hir::{
        Branch, Function, Instruction, InstructionIdx, LocalIdx, LocalStorage, Operand, Terminator,
        op::BinOp,
    },
    ty::{self, Ty, TyIdx},
};
use index_vec::{IndexVec, define_index_type};

define_index_type! {
    pub struct BlockIdx = usize;
}

#[derive(Debug)]
pub struct BasicBlock {
    pub name: String,
    pub instructions: IndexVec<InstructionIdx, Instruction>,
    pub terminator: Terminator,
}

impl BasicBlock {
    pub fn new(name: String) -> Self {
        Self {
            name,
            instructions: IndexVec::new(),
            terminator: Terminator::Return(None),
        }
    }
}

pub struct Builder<'a> {
    pub func: &'a mut Function,
    pub ty_storage: &'a ty::Storage,

    idx: BlockIdx,
}

impl<'a> Builder<'a> {
    pub fn new(func: &'a mut Function, ty_storage: &'a ty::Storage, idx: BlockIdx) -> Self {
        Self {
            func,
            ty_storage,
            idx,
        }
    }

    pub fn create_ret(&mut self, value: Option<Operand>) {
        self.get_bb_mut().terminator = Terminator::Return(value);
    }

    pub fn create_cond_br(&mut self, condition: Operand, iftrue: BlockIdx, iffalse: BlockIdx) {
        self.get_bb_mut().terminator = Terminator::Br(Branch::Conditional {
            condition,
            iftrue,
            iffalse,
        });
    }

    pub fn create_br(&mut self, block_idx: BlockIdx) {
        self.get_bb_mut().terminator = Terminator::Br(Branch::Unconditional { block_idx });
    }

    pub fn create_bin(&mut self, lhs: Operand, rhs: Operand, kind: BinOp) -> Operand {
        assert!(lhs.ty(self) == rhs.ty(self));
        let idx = self.create_local(lhs.ty(self));

        self.get_bb_mut().instructions.push(Instruction::Binary {
            kind,
            lhs,
            rhs,
            out: idx,
        });

        Operand::Local(idx)
    }

    pub fn create_sext(&mut self, operand: Operand) -> Operand {
        let idx = self.create_local(operand.ty(self));

        self.get_bb_mut()
            .instructions
            .push(Instruction::Sext { operand, out: idx });

        Operand::Local(idx)
    }

    pub fn create_zext(&mut self, operand: Operand) -> Operand {
        let idx = self.create_local(operand.ty(self));

        self.get_bb_mut()
            .instructions
            .push(Instruction::Zext { operand, out: idx });

        Operand::Local(idx)
    }

    pub fn create_alloca(&mut self, ty: TyIdx) -> Operand {
        let idx = self.create_local(self.ty_storage.ptr_ty);

        self.get_bb_mut()
            .instructions
            .push(Instruction::Alloca { ty, out: idx });

        Operand::Local(idx)
    }

    pub fn create_store(&mut self, ptr: Operand, value: Operand) {
        self.get_bb_mut()
            .instructions
            .push(Instruction::Store { ptr, value });
    }

    pub fn create_load(&mut self, ptr: Operand, ty: TyIdx) -> Operand {
        let idx = self.create_local(ty);

        self.get_bb_mut()
            .instructions
            .push(Instruction::Load { ptr, out: idx });

        Operand::Local(idx)
    }

    pub fn create_gep(&mut self, ptr_ty: TyIdx, ptr: Operand, indices: Vec<Operand>) -> Operand {
        let idx = self.create_local(self.ty_storage.ptr_ty);

        self.get_bb_mut()
            .instructions
            .push(Instruction::GetElementPtr {
                ptr_ty,
                ptr,
                indices,
                out: idx,
            });

        Operand::Local(idx)
    }

    pub fn create_icmp(&mut self, lhs: Operand, rhs: Operand, cond_code: ConditionCode) -> Operand {
        assert!(lhs.ty(self) == rhs.ty(self));
        let idx = self.create_local(self.ty_storage.i8_ty);

        self.get_bb_mut().instructions.push(Instruction::Icmp {
            cond_code,
            lhs,
            rhs,
            out: idx,
        });

        Operand::Local(idx)
    }

    pub fn create_call(
        &mut self,
        operand: Operand,
        ret_ty: TyIdx,
        arguments: Vec<Operand>,
    ) -> Option<Operand> {
        let idx = if self.ty_storage.get_ty(ret_ty) == &Ty::Void {
            None
        } else {
            Some(self.create_local(ret_ty))
        };

        self.get_bb_mut().instructions.push(Instruction::Call {
            operand,
            arguments,
            out: idx,
        });

        idx.map(|idx| Operand::Local(idx))
    }

    fn get_bb_mut(&mut self) -> &mut BasicBlock {
        &mut self.func.blocks[self.idx]
    }

    fn create_local(&mut self, ty: TyIdx) -> LocalIdx {
        self.func.locals.push(ty)
    }
}

impl LocalStorage for Builder<'_> {
    fn get_local_ty(&self, idx: LocalIdx) -> TyIdx {
        self.func.locals[idx]
    }
}

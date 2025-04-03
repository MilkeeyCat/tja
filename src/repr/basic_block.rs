use super::{
    Branch, Instruction, LocalIdx, LocalStorage, Operand, Terminator,
    op::{BinOp, CmpOp},
    ty::{self, TyIdx},
};

pub type BlockIdx = usize;

#[derive(Debug)]
pub struct BasicBlock {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

impl BasicBlock {
    pub fn new(name: String) -> Self {
        Self {
            name,
            instructions: Vec::new(),
            terminator: Terminator::Return(None),
        }
    }
}

pub struct Wrapper<'ctx> {
    pub ty_storage: &'ctx mut ty::Storage,
    pub fn_locals: &'ctx mut Vec<TyIdx>,
    pub block: &'ctx mut BasicBlock,
}

impl<'ctx> Wrapper<'ctx> {
    pub fn create_ret(&mut self, value: Option<Operand>) {
        self.block.terminator = Terminator::Return(value);
    }

    pub fn create_cond_br(&mut self, condition: Operand, iftrue: BlockIdx, iffalse: BlockIdx) {
        self.block.terminator = Terminator::Br(Branch::Conditional {
            condition,
            iftrue,
            iffalse,
        });
    }

    pub fn create_br(&mut self, block_idx: BlockIdx) {
        self.block.terminator = Terminator::Br(Branch::Unconditional { block_idx });
    }

    pub fn create_bin(&mut self, lhs: Operand, rhs: Operand, kind: BinOp) -> Operand {
        assert!(lhs.ty(self) == rhs.ty(self));
        let idx = self.create_local(lhs.ty(self));
        self.block.instructions.push(Instruction::Binary {
            kind,
            lhs,
            rhs,
            out: idx,
        });

        Operand::Local(idx)
    }

    pub fn create_sext(&mut self, operand: Operand) -> Operand {
        let idx = self.create_local(operand.ty(self));
        self.block
            .instructions
            .push(Instruction::Sext { operand, out: idx });

        Operand::Local(idx)
    }

    pub fn create_zext(&mut self, operand: Operand) -> Operand {
        let idx = self.create_local(operand.ty(self));
        self.block
            .instructions
            .push(Instruction::Zext { operand, out: idx });

        Operand::Local(idx)
    }

    pub fn create_copy(&mut self, operand: Operand) -> Operand {
        let idx = self.create_local(operand.ty(self));
        self.block
            .instructions
            .push(Instruction::Copy { operand, out: idx });

        Operand::Local(idx)
    }

    pub fn create_alloca(&mut self, ty: TyIdx) -> Operand {
        let idx = self.create_local(ty);
        self.block
            .instructions
            .push(Instruction::Alloca { ty, out: idx });

        Operand::Local(idx)
    }

    pub fn create_store(&mut self, ptr: Operand, value: Operand) {
        self.block
            .instructions
            .push(Instruction::Store { ptr, value });
    }

    pub fn create_load(&mut self, ptr: Operand, ty: TyIdx) -> Operand {
        let idx = self.create_local(ty);
        self.block
            .instructions
            .push(Instruction::Load { ptr, out: idx });

        Operand::Local(idx)
    }

    pub fn create_gep(&mut self, ptr_ty: TyIdx, ptr: Operand, indices: Vec<Operand>) -> Operand {
        let idx = self.create_local(self.ty_storage.ptr_ty);
        self.block.instructions.push(Instruction::GetElementPtr {
            ptr_ty,
            ptr,
            indices,
            out: idx,
        });

        Operand::Local(idx)
    }

    pub fn create_icmp(&mut self, lhs: Operand, rhs: Operand, kind: CmpOp) -> Operand {
        assert!(lhs.ty(self) == rhs.ty(self));
        let idx = self.create_local(self.ty_storage.i8_ty);
        self.block.instructions.push(Instruction::Icmp {
            kind,
            lhs,
            rhs,
            out: idx,
        });

        Operand::Local(idx)
    }

    fn create_local(&mut self, ty: TyIdx) -> LocalIdx {
        let idx = self.fn_locals.len();
        self.fn_locals.push(ty);

        idx
    }
}

impl LocalStorage for Wrapper<'_> {
    fn get_local_ty(&self, idx: LocalIdx) -> TyIdx {
        self.fn_locals[idx]
    }
}

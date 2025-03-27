use super::{Instruction, LocalIdx, LocalStorage, Operand, Terminator, op::BinOp, ty::Ty};

#[derive(Debug)]
pub struct BasicBlock {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

pub struct Builder<'f> {
    pub fn_locals: &'f mut Vec<Ty>,
    pub block: &'f mut BasicBlock,
}

impl<'f> Builder<'f> {
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

    pub fn create_alloca(&mut self, ty: Ty) -> Operand {
        let idx = self.create_local(ty.clone());
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

    pub fn create_load(&mut self, ptr: Operand, ty: Ty) -> Operand {
        let idx = self.create_local(ty);
        self.block
            .instructions
            .push(Instruction::Load { ptr, out: idx });

        Operand::Local(idx)
    }

    pub fn create_gep(&mut self, ptr_ty: Ty, ptr: Operand, indices: Vec<Operand>) -> Operand {
        let idx = self.create_local(Ty::Ptr);
        self.block.instructions.push(Instruction::GetElementPtr {
            ptr_ty,
            ptr,
            indices,
            out: idx,
        });

        Operand::Local(idx)
    }

    fn create_local(&mut self, ty: Ty) -> LocalIdx {
        let idx = self.fn_locals.len();
        self.fn_locals.push(ty);

        idx
    }
}

impl LocalStorage for Builder<'_> {
    fn get_local_ty(&self, idx: super::LocalIdx) -> &Ty {
        &self.fn_locals[idx]
    }
}

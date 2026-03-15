use crate::{
    hir::{LocalIdx, passes::lower::FnLowering},
    mir::{self, GenericInstruction, GenericRegister, VregIdx, instruction as mir_instr},
    targets::{
        Abi, CallingConvention, Target,
        amd64::{self, AddressMode, Base, ReadWrite, Register, instruction as amd64_instr},
    },
    ty::{self, Ty, TyIdx},
};

#[derive(Debug, PartialEq, Clone, Copy)]
enum ClassKind {
    Integer,
    Memory,
    Sse,
    SseUp,
    NoClass,
}

type AbiOf<C> = <<C as CallingConvention>::Target as Target>::Abi;

#[derive(Default)]
pub struct SysV;

impl SysV {
    fn classify(
        &self,
        ty_storage: &ty::Storage,
        ty: TyIdx,
        offset: usize,
        eightbytes: &mut [ClassKind],
    ) {
        match ty_storage.get_ty(ty) {
            Ty::Void => (),
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::Ptr => {
                let idx = offset / 8;

                eightbytes[idx] = self.merge(eightbytes[idx], ClassKind::Integer);
            }
            Ty::Struct(tys) => {
                for (i, &ty) in tys.iter().enumerate() {
                    self.classify(
                        ty_storage,
                        ty,
                        AbiOf::<Self>::field_offset(ty_storage, &tys, i),
                        eightbytes,
                    );
                }
            }
            Ty::Array { ty, len } => {
                for i in 0..*len {
                    self.classify(
                        ty_storage,
                        *ty,
                        AbiOf::<Self>::ty_size(ty_storage, *ty) * i,
                        eightbytes,
                    );
                }
            }
        };
    }

    fn ty_class(&self, storage: &ty::Storage, ty: TyIdx) -> Vec<ClassKind> {
        let size = AbiOf::<Self>::ty_size(storage, ty);
        let mut eightbytes = vec![ClassKind::NoClass; size.next_multiple_of(8) / 8];

        if eightbytes.len() > 8 {
            eightbytes[0] = ClassKind::Memory;
        } else {
            self.classify(storage, ty, 0, &mut eightbytes);
        }

        // post merger cleanup
        {
            // 5.a
            if eightbytes.contains(&ClassKind::Memory) {
                eightbytes.clear();
                eightbytes.push(ClassKind::Memory);
            }

            // 5.c
            if eightbytes.len() > 2
                && (eightbytes[0] != ClassKind::Sse
                    || eightbytes[1..].iter().any(|kind| kind != &ClassKind::SseUp))
            {
                eightbytes.clear();
                eightbytes.push(ClassKind::Memory);
            }
        }

        eightbytes
    }

    fn merge(&self, lhs: ClassKind, rhs: ClassKind) -> ClassKind {
        match (lhs, rhs) {
            (lhs, rhs) if lhs == rhs => lhs,
            (ClassKind::NoClass, other) | (other, ClassKind::NoClass) => other,
            (ClassKind::Memory, other) | (other, ClassKind::Memory) => other,
            (ClassKind::Integer, other) | (other, ClassKind::Integer) => other,
            _ => ClassKind::Sse,
        }
    }
}

impl CallingConvention for SysV {
    type Target = super::Target;

    fn lower_ret(
        &self,
        lowering: &mut FnLowering<Self::Target>,
        operand: Option<(Vec<mir::VregIdx>, TyIdx)>,
    ) {
        if let Some((mut vreg_indices, ty)) = operand {
            let mut registers = vec![Register::Rdx, Register::Rax];
            let mut offsets = lowering.ty_to_offsets[&ty].clone();

            assert!(vreg_indices.len() == offsets.len());

            for class in self.ty_class(lowering.ty_storage, ty).into_iter() {
                match class {
                    ClassKind::Memory => {
                        let vreg_idx: usize = (0..lowering.hir_function.params_count)
                            .into_iter()
                            .map(|local_idx| {
                                lowering
                                    .get_or_create_vregs(LocalIdx::new(local_idx).into())
                                    .len()
                            })
                            .sum();

                        assert_eq!(
                            lowering.mir_function.vreg_info.get_vreg(vreg_idx.into()).ty,
                            lowering.ty_storage.ptr_ty
                        );

                        let base = lowering
                            .mir_function
                            .vreg_info
                            .create_vreg(lowering.ty_storage.ptr_ty);

                        let instr_idx =
                            lowering
                                .mir_function
                                .create_instr(GenericInstruction::Target(
                                    amd64::Instruction::Target(
                                        amd64_instr::Lea64::new(
                                            base.into(),
                                            AddressMode {
                                                base: Base::Register(GenericRegister::Virtual(
                                                    vreg_idx.into(),
                                                )),
                                                index: None,
                                                scale: 1,
                                                displacement: None,
                                            },
                                        )
                                        .into(),
                                    ),
                                ));

                        lowering.instr_cursor_mut().insert_after(instr_idx);

                        for (vreg_idx, offset) in std::mem::take(&mut vreg_indices)
                            .into_iter()
                            .zip(std::mem::take(&mut offsets))
                        {
                            let ptr_add = lowering
                                .mir_function
                                .vreg_info
                                .create_vreg(lowering.ty_storage.ptr_ty);
                            let ptr_add_instr_idx =
                                lowering.mir_function.create_instr(mir_instr::PtrAdd::new(
                                    ptr_add.into(),
                                    base.into(),
                                    (offset as i64).into(),
                                ));
                            let store_instr_idx = lowering.mir_function.create_instr(
                                mir_instr::Store::new(vreg_idx.into(), ptr_add.into()),
                            );
                            let mut cursor = lowering.instr_cursor_mut();

                            cursor.insert_after(ptr_add_instr_idx);
                            cursor.insert_after(store_instr_idx);
                        }
                    }
                    ClassKind::Integer => {
                        let reg = registers.pop().unwrap();
                        let (vreg_indices, offsets) =
                            get_vregs_for_one_reg(lowering, &mut vreg_indices, &mut offsets);
                        let mut last_offset = None;

                        for (vreg_idx, offset) in vreg_indices.into_iter().zip(offsets).rev() {
                            let ty = lowering.mir_function.vreg_info.get_vreg(vreg_idx).ty;
                            let ty_size = AbiOf::<Self>::ty_size(lowering.ty_storage, ty);

                            if let Some(last_offset) = last_offset {
                                let instr_idx =
                                    lowering
                                        .mir_function
                                        .create_instr(GenericInstruction::Target(
                                            amd64::Instruction::Target(
                                                amd64_instr::Shl64r8i::new(
                                                    ReadWrite::new(reg.into(), reg.into()),
                                                    (last_offset - offset) as i64 * 8,
                                                )
                                                .into(),
                                            ),
                                        ));

                                lowering.instr_cursor_mut().insert_after(instr_idx);
                            }

                            let reg = match (reg, ty_size) {
                                (Register::Rax, 8) => Register::Rax,
                                (Register::Rax, 4) => Register::Eax,
                                (Register::Rax, 2) => Register::Ax,
                                (Register::Rax, 1) => Register::Al,

                                (Register::Rdx, 8) => Register::Rdx,
                                (Register::Rdx, 4) => Register::Edx,
                                (Register::Rdx, 2) => Register::Dx,
                                (Register::Rdx, 1) => Register::Dl,

                                _ => unreachable!(),
                            };

                            let instr_idx =
                                lowering.mir_function.create_instr(mir_instr::Copy::new(
                                    reg.into(),
                                    GenericRegister::Virtual(vreg_idx).into(),
                                ));

                            lowering.instr_cursor_mut().insert_after(instr_idx);
                            last_offset = Some(offset);
                        }
                    }
                    _ => unimplemented!(),
                }
            }

            assert!(vreg_indices.is_empty() && offsets.is_empty());
        }

        let instr_idx = lowering
            .mir_function
            .create_instr(GenericInstruction::Target(amd64::Instruction::Target(
                amd64_instr::Ret::new().into(),
            )));

        lowering.instr_cursor_mut().insert_after(instr_idx);
    }

    fn lower_params(
        &self,
        lowering: &mut FnLowering<Self::Target>,
        vreg_indices: Vec<Vec<VregIdx>>,
        tys: Vec<TyIdx>,
        ret_ty: TyIdx,
    ) {
        assert!(vreg_indices.len() == tys.len());

        let mut registers = vec![
            Register::R9,
            Register::R8,
            Register::Rcx,
            Register::Rdx,
            Register::Rsi,
            Register::Rdi,
        ];

        if self
            .ty_class(lowering.ty_storage, ret_ty)
            .contains(&ClassKind::Memory)
        {
            let vreg_idx = lowering
                .mir_function
                .vreg_info
                .create_vreg(lowering.ty_storage.ptr_ty);
            assert_eq!(
                vreg_idx.raw(),
                vreg_indices
                    .iter()
                    .map(|vreg_indices| vreg_indices.len())
                    .sum::<usize>()
            );

            let instr_idx = lowering.mir_function.create_instr(mir_instr::Copy::new(
                vreg_idx.into(),
                GenericRegister::Physical(Register::Rdi).into(),
            ));

            lowering.instr_cursor_mut().insert_after(instr_idx);
            registers.retain(|reg| reg != &Register::Rdi);
        }

        for ((ty, mut classes), mut vreg_indices) in tys
            .into_iter()
            .map(|ty| (ty, self.ty_class(lowering.ty_storage, ty)))
            .zip(vreg_indices)
        {
            if classes.contains(&ClassKind::Integer) && registers.len() < classes.len() {
                classes = vec![ClassKind::Memory];
            }

            let mut offsets = lowering.ty_to_offsets[&ty].clone();

            for class in classes {
                match class {
                    ClassKind::Memory => {
                        let base = lowering
                            .mir_function
                            .vreg_info
                            .create_vreg(lowering.ty_storage.ptr_ty);
                        let instr_idx =
                            lowering
                                .mir_function
                                .create_instr(GenericInstruction::Target(
                                    amd64::Instruction::Target(
                                        amd64_instr::Lea64::new(
                                            base.into(),
                                            AddressMode {
                                                base: Base::Register(Register::Rbp.into()),
                                                index: None,
                                                scale: 1,
                                                displacement: Some(16), // return address & rbp
                                            },
                                        )
                                        .into(),
                                    ),
                                ));

                        lowering.instr_cursor_mut().insert_after(instr_idx);

                        for (vreg_idx, offset) in std::mem::take(&mut vreg_indices)
                            .into_iter()
                            .zip(std::mem::take(&mut offsets))
                        {
                            let ptr_add = lowering
                                .mir_function
                                .vreg_info
                                .create_vreg(lowering.ty_storage.ptr_ty);
                            let ptr_add_instr_idx =
                                lowering.mir_function.create_instr(mir_instr::PtrAdd::new(
                                    ptr_add.into(),
                                    base.into(),
                                    (offset as i64).into(),
                                ));
                            let load_instr_idx = lowering.mir_function.create_instr(
                                mir_instr::Load::new(vreg_idx.into(), ptr_add.into()),
                            );
                            let mut cursor = lowering.instr_cursor_mut();

                            cursor.insert_after(ptr_add_instr_idx);
                            cursor.insert_after(load_instr_idx);
                        }
                    }
                    ClassKind::Integer => {
                        let reg = registers.pop().unwrap();
                        let (vreg_indices, offsets) =
                            get_vregs_for_one_reg(lowering, &mut vreg_indices, &mut offsets);
                        let mut last_offset = None;

                        for (vreg_idx, offset) in vreg_indices.into_iter().zip(offsets) {
                            let ty = lowering.mir_function.vreg_info.get_vreg(vreg_idx).ty;
                            let ty_size = AbiOf::<Self>::ty_size(lowering.ty_storage, ty);

                            if let Some(last_offset) = last_offset {
                                let instr_idx =
                                    lowering
                                        .mir_function
                                        .create_instr(GenericInstruction::Target(
                                            amd64::Instruction::Target(
                                                amd64_instr::Shr64r8i::new(
                                                    ReadWrite::new(reg.into(), reg.into()),
                                                    (offset - last_offset) as i64 * 8,
                                                )
                                                .into(),
                                            ),
                                        ));

                                lowering.instr_cursor_mut().insert_after(instr_idx);
                            }

                            let reg = match (reg, ty_size) {
                                (Register::R9, 8) => Register::R9,
                                (Register::R9, 4) => Register::R9d,
                                (Register::R9, 2) => Register::R9w,
                                (Register::R9, 1) => Register::R9b,

                                (Register::R8, 8) => Register::R8,
                                (Register::R8, 4) => Register::R8d,
                                (Register::R8, 2) => Register::R8w,
                                (Register::R8, 1) => Register::R8b,

                                (Register::Rcx, 8) => Register::Rcx,
                                (Register::Rcx, 4) => Register::Ecx,
                                (Register::Rcx, 2) => Register::Cx,
                                (Register::Rcx, 1) => Register::Cl,

                                (Register::Rdx, 8) => Register::Rdx,
                                (Register::Rdx, 4) => Register::Edx,
                                (Register::Rdx, 2) => Register::Dx,
                                (Register::Rdx, 1) => Register::Dl,

                                (Register::Rsi, 8) => Register::Rsi,
                                (Register::Rsi, 4) => Register::Esi,
                                (Register::Rsi, 2) => Register::Si,
                                (Register::Rsi, 1) => Register::Sil,

                                (Register::Rdi, 8) => Register::Rdi,
                                (Register::Rdi, 4) => Register::Edi,
                                (Register::Rdi, 2) => Register::Di,
                                (Register::Rdi, 1) => Register::Dil,

                                _ => unreachable!(),
                            };

                            let instr_idx =
                                lowering.mir_function.create_instr(mir_instr::Copy::new(
                                    vreg_idx.into(),
                                    GenericRegister::Physical(reg).into(),
                                ));

                            lowering.instr_cursor_mut().insert_after(instr_idx);
                            last_offset = Some(offset);
                        }
                    }
                    ClassKind::Sse => unimplemented!(),
                    ClassKind::SseUp => unimplemented!(),
                    ClassKind::NoClass => unreachable!(),
                }
            }
        }
    }

    fn lower_call(
        &self,
        lowering: &mut FnLowering<Self::Target>,
        callee_vreg_idx: VregIdx,
        arg_vreg_indices: Vec<Vec<VregIdx>>,
        arg_tys: Vec<TyIdx>,
        ret: Option<(Vec<VregIdx>, TyIdx)>,
    ) {
        assert!(arg_vreg_indices.len() == arg_tys.len());

        let mut registers = vec![
            Register::R9,
            Register::R8,
            Register::Rcx,
            Register::Rdx,
            Register::Rsi,
            Register::Rdi,
        ];

        if let Some((_, ty)) = &ret {
            if self
                .ty_class(lowering.ty_storage, *ty)
                .contains(&ClassKind::Memory)
            {
                registers.retain(|reg| reg != &Register::Rdi);

                let frame_idx = lowering
                    .mir_function
                    .frame_info
                    .create_stack_object(AbiOf::<Self>::ty_size(lowering.ty_storage, *ty));
                let instr_idx = lowering
                    .mir_function
                    .create_instr(mir_instr::FrameIndex::new(Register::Rdi.into(), frame_idx));

                lowering.instr_cursor_mut().insert_after(instr_idx);
            }
        }

        let adj_stack_pos = lowering.instr_cursor().idx().unwrap();
        let mut stack_offset: usize = 0;

        for ((ty, mut classes), mut vreg_indices) in arg_tys
            .into_iter()
            .map(|ty| (ty, self.ty_class(lowering.ty_storage, ty)))
            .zip(arg_vreg_indices)
        {
            if classes.contains(&ClassKind::Integer) && registers.len() < classes.len() {
                classes = vec![ClassKind::Memory];
            }

            let mut offsets = lowering.ty_to_offsets[&ty].clone();

            for class in classes {
                match class {
                    ClassKind::Memory => {
                        let base = GenericRegister::Physical(Register::Rsp);

                        stack_offset = stack_offset
                            .next_multiple_of(AbiOf::<Self>::alignment(lowering.ty_storage, ty));

                        for (vreg_idx, offset) in std::mem::take(&mut vreg_indices)
                            .into_iter()
                            .zip(std::mem::take(&mut offsets))
                        {
                            let ptr_add = lowering
                                .mir_function
                                .vreg_info
                                .create_vreg(lowering.ty_storage.ptr_ty);
                            let ptr_add_instr_idx =
                                lowering.mir_function.create_instr(mir_instr::PtrAdd::new(
                                    ptr_add.into(),
                                    base.clone().into(),
                                    ((stack_offset + offset) as i64).into(),
                                ));
                            let store_instr_idx = lowering.mir_function.create_instr(
                                mir_instr::Store::new(vreg_idx.into(), ptr_add.into()),
                            );
                            let mut cursor = lowering.instr_cursor_mut();

                            cursor.insert_after(ptr_add_instr_idx);
                            cursor.insert_after(store_instr_idx);
                        }

                        stack_offset += AbiOf::<Self>::ty_size(lowering.ty_storage, ty);
                    }
                    ClassKind::Integer => {
                        let reg = registers.pop().unwrap();
                        let (vreg_indices, offsets) =
                            get_vregs_for_one_reg(lowering, &mut vreg_indices, &mut offsets);
                        let mut last_offset = None;

                        for (vreg_idx, offset) in vreg_indices.into_iter().zip(offsets).rev() {
                            let ty = lowering.mir_function.vreg_info.get_vreg(vreg_idx).ty;
                            let ty_size = AbiOf::<Self>::ty_size(lowering.ty_storage, ty);

                            if let Some(last_offset) = last_offset {
                                let instr_idx =
                                    lowering
                                        .mir_function
                                        .create_instr(GenericInstruction::Target(
                                            amd64::Instruction::Target(
                                                amd64_instr::Shl64r8i::new(
                                                    ReadWrite::new(reg.into(), reg.into()),
                                                    (last_offset - offset) as i64 * 8,
                                                )
                                                .into(),
                                            ),
                                        ));

                                lowering.instr_cursor_mut().insert_after(instr_idx);
                            }

                            let reg = match (reg, ty_size) {
                                (Register::R9, 8) => Register::R9,
                                (Register::R9, 4) => Register::R9d,
                                (Register::R9, 2) => Register::R9w,
                                (Register::R9, 1) => Register::R9b,

                                (Register::R8, 8) => Register::R8,
                                (Register::R8, 4) => Register::R8d,
                                (Register::R8, 2) => Register::R8w,
                                (Register::R8, 1) => Register::R8b,

                                (Register::Rcx, 8) => Register::Rcx,
                                (Register::Rcx, 4) => Register::Ecx,
                                (Register::Rcx, 2) => Register::Cx,
                                (Register::Rcx, 1) => Register::Cl,

                                (Register::Rdx, 8) => Register::Rdx,
                                (Register::Rdx, 4) => Register::Edx,
                                (Register::Rdx, 2) => Register::Dx,
                                (Register::Rdx, 1) => Register::Dl,

                                (Register::Rsi, 8) => Register::Rsi,
                                (Register::Rsi, 4) => Register::Esi,
                                (Register::Rsi, 2) => Register::Si,
                                (Register::Rsi, 1) => Register::Sil,

                                (Register::Rdi, 8) => Register::Rdi,
                                (Register::Rdi, 4) => Register::Edi,
                                (Register::Rdi, 2) => Register::Di,
                                (Register::Rdi, 1) => Register::Dil,

                                _ => unreachable!(),
                            };

                            let instr_idx =
                                lowering.mir_function.create_instr(mir_instr::Copy::new(
                                    reg.into(),
                                    GenericRegister::Virtual(vreg_idx).into(),
                                ));

                            lowering.instr_cursor_mut().insert_after(instr_idx);
                            last_offset = Some(offset);
                        }
                    }
                    _ => unimplemented!(),
                }
            }
        }

        if stack_offset > 0 {
            let instr_idx = lowering
                .mir_function
                .create_instr(GenericInstruction::Target(amd64::Instruction::Target(
                    amd64_instr::Sub64ri32::new(
                        ReadWrite::new(Register::Rsp.into(), Register::Rsp.into()),
                        stack_offset as i64,
                    )
                    .into(),
                )));
            let mut cursor = lowering.instr_cursor_mut();

            cursor.set_idx(adj_stack_pos);
            cursor.insert_after(instr_idx);
        }

        let instr_idx = lowering
            .mir_function
            .create_instr(GenericInstruction::Target(
                amd64_instr::Call::new(
                    callee_vreg_idx.into(),
                    AbiOf::<Self>::caller_saved_regs().to_vec(),
                )
                .into(),
            ));

        //for reg in lowering.abi.caller_saved_regs() {
        //    instr_builder.add_implicit_def(mir::Register::Physical(*reg));
        //}

        lowering.instr_cursor_mut().insert_after(instr_idx);

        if stack_offset > 0 {
            let instr_idx = lowering
                .mir_function
                .create_instr(GenericInstruction::Target(amd64::Instruction::Target(
                    amd64_instr::Add64ri32::new(
                        ReadWrite::new(Register::Rsp.into(), Register::Rsp.into()),
                        stack_offset as i64,
                    )
                    .into(),
                )));

            lowering.instr_cursor_mut().insert_after(instr_idx);
        }

        if let Some((mut vreg_indices, ty)) = ret {
            let mut registers = vec![Register::Rdx, Register::Rax];
            let mut offsets = lowering.ty_to_offsets[&ty].clone();

            for class in self.ty_class(lowering.ty_storage, ty) {
                match class {
                    ClassKind::Memory => {
                        let base = lowering
                            .mir_function
                            .vreg_info
                            .create_vreg(lowering.ty_storage.ptr_ty);
                        let instr_idx =
                            lowering
                                .mir_function
                                .create_instr(GenericInstruction::Target(
                                    amd64::Instruction::Target(
                                        amd64_instr::Lea64::new(
                                            base.into(),
                                            AddressMode {
                                                base: Base::Register(Register::Rdi.into()),
                                                index: None,
                                                scale: 1,
                                                displacement: None,
                                            },
                                        )
                                        .into(),
                                    ),
                                ));

                        lowering.instr_cursor_mut().insert_after(instr_idx);

                        for (vreg_idx, offset) in std::mem::take(&mut vreg_indices)
                            .into_iter()
                            .zip(std::mem::take(&mut offsets))
                        {
                            let ptr_add = lowering
                                .mir_function
                                .vreg_info
                                .create_vreg(lowering.ty_storage.ptr_ty);
                            let ptr_add_instr_idx =
                                lowering.mir_function.create_instr(mir_instr::PtrAdd::new(
                                    ptr_add.into(),
                                    base.into(),
                                    (offset as i64).into(),
                                ));
                            let load_instr_idx = lowering.mir_function.create_instr(
                                mir_instr::Load::new(vreg_idx.into(), ptr_add.into()),
                            );
                            let mut cursor = lowering.instr_cursor_mut();

                            cursor.insert_after(ptr_add_instr_idx);
                            cursor.insert_after(load_instr_idx);
                        }
                    }
                    ClassKind::Integer => {
                        let reg = registers.pop().unwrap();
                        let (vreg_indices, offsets) =
                            get_vregs_for_one_reg(lowering, &mut vreg_indices, &mut offsets);
                        let mut last_offset = None;

                        for (vreg_idx, offset) in vreg_indices.into_iter().zip(offsets) {
                            let ty = lowering.mir_function.vreg_info.get_vreg(vreg_idx).ty;
                            let ty_size = AbiOf::<Self>::ty_size(lowering.ty_storage, ty);

                            if let Some(last_offset) = last_offset {
                                let instr_idx =
                                    lowering
                                        .mir_function
                                        .create_instr(GenericInstruction::Target(
                                            amd64::Instruction::Target(
                                                amd64_instr::Shr64r8i::new(
                                                    ReadWrite::new(reg.into(), reg.into()),
                                                    (offset - last_offset) as i64 * 8,
                                                )
                                                .into(),
                                            ),
                                        ));

                                lowering.instr_cursor_mut().insert_after(instr_idx);
                            }

                            let reg = match (reg, ty_size) {
                                (Register::Rax, 8) => Register::Rax,
                                (Register::Rax, 4) => Register::Eax,
                                (Register::Rax, 2) => Register::Ax,
                                (Register::Rax, 1) => Register::Al,

                                (Register::Rdx, 8) => Register::Rdx,
                                (Register::Rdx, 4) => Register::Edx,
                                (Register::Rdx, 2) => Register::Dx,
                                (Register::Rdx, 1) => Register::Dl,

                                _ => unreachable!(),
                            };

                            let instr_idx =
                                lowering.mir_function.create_instr(mir_instr::Copy::new(
                                    vreg_idx.into(),
                                    GenericRegister::Physical(reg).into(),
                                ));

                            lowering.instr_cursor_mut().insert_after(instr_idx);
                            last_offset = Some(offset);
                        }
                    }
                    _ => unimplemented!(),
                }
            }
        }
    }
}

fn get_vregs_for_one_reg<T: Target>(
    lowering: &FnLowering<'_, T>,
    vreg_indices: &mut Vec<VregIdx>,
    offsets: &mut Vec<usize>,
) -> (Vec<VregIdx>, Vec<usize>) {
    let mut result_vregs = Vec::new();
    let mut result_offsets = Vec::new();
    let mut last_offset_plus_size = 0;

    loop {
        if vreg_indices.is_empty() {
            break;
        }

        let vreg_idx = vreg_indices[0];
        let ty = lowering.mir_function.vreg_info.get_vreg(vreg_idx).ty;
        let ty_size = T::Abi::ty_size(lowering.ty_storage, ty);

        if offsets[0] + ty_size > 8 {
            break;
        }

        last_offset_plus_size = offsets[0] + ty_size;
        result_vregs.push(vreg_indices.remove(0));
        result_offsets.push(offsets.remove(0));
    }

    for offset in offsets {
        *offset -= last_offset_plus_size;
    }

    (result_vregs, result_offsets)
}

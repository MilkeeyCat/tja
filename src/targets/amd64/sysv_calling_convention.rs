use crate::{
    hir::ty::{self, Ty, TyIdx},
    lowering::FnLowering,
    mir::{self, GenericOpcode, Instruction, Operand, PhysicalRegister, RegisterRole, VregIdx},
    targets::{
        Abi, CallingConvention,
        amd64::{
            Opcode, Register,
            address_mode::{AddressMode, Base},
        },
    },
};

#[derive(Debug, PartialEq, Clone, Copy)]
enum ClassKind {
    Integer,
    Memory,
    Sse,
    SseUp,
    NoClass,
}

pub struct SysVAmd64;

impl SysVAmd64 {
    pub fn new() -> Self {
        Self
    }

    fn classify<A: Abi>(
        &self,
        abi: &A,
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
                        abi,
                        ty_storage,
                        ty,
                        abi.field_offset(ty_storage, &tys, i),
                        eightbytes,
                    );
                }
            }
        };
    }

    fn ty_class<A: Abi>(&self, abi: &A, storage: &ty::Storage, ty: TyIdx) -> Vec<ClassKind> {
        let size = abi.ty_size(storage, ty);
        let mut eightbytes = vec![ClassKind::NoClass; size.next_multiple_of(8) / 8];

        if eightbytes.len() > 8 {
            eightbytes[0] = ClassKind::Memory;
        } else {
            self.classify(abi, storage, ty, 0, &mut eightbytes);
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

impl CallingConvention for SysVAmd64 {
    fn lower_ret<A: Abi>(
        &self,
        lowering: &mut FnLowering<A>,
        mut vreg_indices: Vec<mir::VregIdx>,
        ty: TyIdx,
    ) {
        let mut registers = vec![Register::Rdx, Register::Rax];
        let mut offsets = lowering.ty_to_offsets[&ty].clone();

        assert!(vreg_indices.len() == offsets.len());

        for class in self
            .ty_class(lowering.abi, lowering.ty_storage, ty)
            .into_iter()
        {
            match class {
                ClassKind::Memory => {
                    let base = lowering.create_vreg(lowering.ty_storage.ptr_ty);
                    let address_mode = AddressMode {
                        base: Base::Register(mir::Register::Physical(
                            Register::Rdi as PhysicalRegister,
                        )),
                        index: None,
                        scale: 1,
                        displacement: None,
                    };
                    let mut operands = vec![mir::Operand::Register(
                        mir::Register::Virtual(base),
                        RegisterRole::Def,
                    )];

                    address_mode.write(&mut operands, 1);
                    lowering
                        .get_basic_block()
                        .instructions
                        .push(mir::Instruction::new(
                            Opcode::Lea64 as mir::Opcode,
                            operands,
                        ));

                    for (vreg_idx, offset) in std::mem::take(&mut vreg_indices)
                        .into_iter()
                        .zip(std::mem::take(&mut offsets))
                    {
                        let ptr_add = lowering.create_vreg(lowering.ty_storage.ptr_ty);
                        let bb = lowering.get_basic_block();

                        bb.instructions.push(mir::Instruction::new(
                            GenericOpcode::PtrAdd as mir::Opcode,
                            vec![
                                mir::Operand::Register(
                                    mir::Register::Virtual(ptr_add),
                                    RegisterRole::Def,
                                ),
                                mir::Operand::Register(
                                    mir::Register::Virtual(base),
                                    RegisterRole::Use,
                                ),
                                mir::Operand::Immediate(offset as u64),
                            ],
                        ));
                        bb.instructions.push(Instruction::new(
                            GenericOpcode::Store as mir::Opcode,
                            vec![
                                Operand::Register(
                                    mir::Register::Virtual(vreg_idx),
                                    RegisterRole::Use,
                                ),
                                Operand::Register(
                                    mir::Register::Virtual(ptr_add),
                                    RegisterRole::Use,
                                ),
                            ],
                        ));
                    }
                }
                ClassKind::Integer => {
                    let r = registers.pop().unwrap();
                    let (vreg_indices, offsets) =
                        get_vregs_for_one_reg(lowering, &mut vreg_indices, &mut offsets);
                    let mut last_offset = None;

                    for (vreg_idx, offset) in vreg_indices.into_iter().zip(offsets).rev() {
                        let ty = lowering.mir_function.vreg_types[&vreg_idx];
                        let ty_size = lowering.abi.ty_size(lowering.ty_storage, ty);

                        if let Some(last_offset) = last_offset {
                            lowering
                                .get_basic_block()
                                .instructions
                                .push(Instruction::new(
                                    Opcode::Shl64ri as mir::Opcode,
                                    vec![
                                        Operand::Register(
                                            mir::Register::Physical(r as PhysicalRegister),
                                            RegisterRole::Use,
                                        ),
                                        Operand::Immediate((last_offset - offset) as u64 * 8),
                                    ],
                                ));
                        }

                        let r = match (r, ty_size) {
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

                        lowering
                            .get_basic_block()
                            .instructions
                            .push(Instruction::new(
                                GenericOpcode::Copy as mir::Opcode,
                                vec![
                                    Operand::Register(
                                        mir::Register::Physical(r as PhysicalRegister),
                                        RegisterRole::Def,
                                    ),
                                    Operand::Register(
                                        mir::Register::Virtual(vreg_idx),
                                        RegisterRole::Use,
                                    ),
                                ],
                            ));
                        last_offset = Some(offset);
                    }
                }
                _ => unimplemented!(),
            }
        }

        assert!(vreg_indices.is_empty() && offsets.is_empty());
    }

    fn lower_params<A: Abi>(
        &self,
        lowering: &mut FnLowering<A>,
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
            .ty_class(lowering.abi, lowering.ty_storage, ret_ty)
            .contains(&ClassKind::Memory)
        {
            registers.retain(|r| r != &Register::Rdx);
        }

        for ((ty, mut classes), mut vreg_indices) in tys
            .into_iter()
            .map(|ty| (ty, self.ty_class(lowering.abi, lowering.ty_storage, ty)))
            .zip(vreg_indices)
        {
            if classes.contains(&ClassKind::Integer) && registers.len() < classes.len() {
                classes = vec![ClassKind::Memory];
            }

            let mut offsets = lowering.ty_to_offsets[&ty].clone();

            for class in classes {
                match class {
                    ClassKind::Memory => {
                        let base = lowering.create_vreg(lowering.ty_storage.ptr_ty);
                        let address_mode = AddressMode {
                            base: Base::Register(mir::Register::Physical(
                                Register::Rbp as PhysicalRegister,
                            )),
                            index: None,
                            scale: 1,
                            displacement: Some(16), // return address & rbp
                        };
                        let mut operands = vec![mir::Operand::Register(
                            mir::Register::Virtual(base),
                            RegisterRole::Def,
                        )];

                        address_mode.write(&mut operands, 1);
                        lowering
                            .get_basic_block()
                            .instructions
                            .push(mir::Instruction::new(
                                Opcode::Lea64 as mir::Opcode,
                                operands,
                            ));

                        for (vreg_idx, offset) in std::mem::take(&mut vreg_indices)
                            .into_iter()
                            .zip(std::mem::take(&mut offsets))
                        {
                            let ptr_add = lowering.create_vreg(lowering.ty_storage.ptr_ty);
                            let bb = lowering.get_basic_block();

                            bb.instructions.push(mir::Instruction::new(
                                GenericOpcode::PtrAdd as mir::Opcode,
                                vec![
                                    mir::Operand::Register(
                                        mir::Register::Virtual(ptr_add),
                                        RegisterRole::Def,
                                    ),
                                    mir::Operand::Register(
                                        mir::Register::Virtual(base),
                                        RegisterRole::Use,
                                    ),
                                    mir::Operand::Immediate(offset as u64),
                                ],
                            ));
                            bb.instructions.push(Instruction::new(
                                GenericOpcode::Load as mir::Opcode,
                                vec![
                                    Operand::Register(
                                        mir::Register::Virtual(vreg_idx),
                                        RegisterRole::Def,
                                    ),
                                    Operand::Register(
                                        mir::Register::Virtual(ptr_add),
                                        RegisterRole::Use,
                                    ),
                                ],
                            ));
                        }
                    }
                    ClassKind::Integer => {
                        let r = registers.pop().unwrap();
                        let (vreg_indices, offsets) =
                            get_vregs_for_one_reg(lowering, &mut vreg_indices, &mut offsets);
                        let mut last_offset = None;

                        for (vreg_idx, offset) in vreg_indices.into_iter().zip(offsets) {
                            let ty = lowering.mir_function.vreg_types[&vreg_idx];
                            let ty_size = lowering.abi.ty_size(lowering.ty_storage, ty);
                            let bb = lowering.get_basic_block();

                            if let Some(last_offset) = last_offset {
                                bb.instructions.push(Instruction::new(
                                    Opcode::Shr64ri as mir::Opcode,
                                    vec![
                                        Operand::Register(
                                            mir::Register::Physical(r as PhysicalRegister),
                                            RegisterRole::Use,
                                        ),
                                        Operand::Immediate((offset - last_offset) as u64 * 8),
                                    ],
                                ));
                            }

                            let r = match (r, ty_size) {
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

                            bb.instructions.push(Instruction::new(
                                GenericOpcode::Copy as mir::Opcode,
                                vec![
                                    Operand::Register(
                                        mir::Register::Virtual(vreg_idx),
                                        RegisterRole::Def,
                                    ),
                                    Operand::Register(
                                        mir::Register::Physical(r as PhysicalRegister),
                                        RegisterRole::Use,
                                    ),
                                ],
                            ));

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

    fn lower_call<A: Abi>(
        &self,
        lowering: &mut FnLowering<A>,
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
                .ty_class(lowering.abi, lowering.ty_storage, *ty)
                .contains(&ClassKind::Memory)
            {
                registers.retain(|r| r != &Register::Rdi);
                let frame_idx = lowering.create_frame_idx(*ty);

                lowering
                    .get_basic_block()
                    .instructions
                    .push(Instruction::new(
                        GenericOpcode::FrameIndex as mir::Opcode,
                        vec![
                            Operand::Register(
                                mir::Register::Physical(Register::Rdi as PhysicalRegister),
                                RegisterRole::Def,
                            ),
                            Operand::Frame(frame_idx),
                        ],
                    ));
            }
        }

        let adj_stack_pos = lowering.get_basic_block().instructions.len();
        let mut stack_offset: usize = 0;

        for ((ty, mut classes), mut vreg_indices) in arg_tys
            .into_iter()
            .map(|ty| (ty, self.ty_class(lowering.abi, lowering.ty_storage, ty)))
            .zip(arg_vreg_indices)
        {
            if classes.contains(&ClassKind::Integer) && registers.len() < classes.len() {
                classes = vec![ClassKind::Memory];
            }

            let mut offsets = lowering.ty_to_offsets[&ty].clone();

            for class in classes {
                match class {
                    ClassKind::Memory => {
                        let base = mir::Register::Physical(Register::Rsp as PhysicalRegister);

                        stack_offset = stack_offset
                            .next_multiple_of(lowering.abi.alignment(lowering.ty_storage, ty));

                        for (vreg_idx, offset) in std::mem::take(&mut vreg_indices)
                            .into_iter()
                            .zip(std::mem::take(&mut offsets))
                        {
                            let ptr_add = lowering.create_vreg(lowering.ty_storage.ptr_ty);
                            let bb = lowering.get_basic_block();

                            bb.instructions.push(mir::Instruction::new(
                                GenericOpcode::PtrAdd as mir::Opcode,
                                vec![
                                    mir::Operand::Register(
                                        mir::Register::Virtual(ptr_add),
                                        RegisterRole::Def,
                                    ),
                                    mir::Operand::Register(base.clone(), RegisterRole::Use),
                                    mir::Operand::Immediate((stack_offset + offset) as u64),
                                ],
                            ));
                            bb.instructions.push(Instruction::new(
                                GenericOpcode::Store as mir::Opcode,
                                vec![
                                    Operand::Register(
                                        mir::Register::Virtual(vreg_idx),
                                        RegisterRole::Use,
                                    ),
                                    Operand::Register(
                                        mir::Register::Virtual(ptr_add),
                                        RegisterRole::Use,
                                    ),
                                ],
                            ));
                        }

                        stack_offset += lowering.abi.ty_size(lowering.ty_storage, ty);
                    }
                    ClassKind::Integer => {
                        let r = registers.pop().unwrap();
                        let (vreg_indices, offsets) =
                            get_vregs_for_one_reg(lowering, &mut vreg_indices, &mut offsets);
                        let mut last_offset = None;

                        for (vreg_idx, offset) in vreg_indices.into_iter().zip(offsets).rev() {
                            let ty = lowering.mir_function.vreg_types[&vreg_idx];
                            let ty_size = lowering.abi.ty_size(lowering.ty_storage, ty);

                            if let Some(last_offset) = last_offset {
                                lowering
                                    .get_basic_block()
                                    .instructions
                                    .push(Instruction::new(
                                        Opcode::Shl64ri as mir::Opcode,
                                        vec![
                                            Operand::Register(
                                                mir::Register::Physical(r as PhysicalRegister),
                                                RegisterRole::Use,
                                            ),
                                            Operand::Immediate((last_offset - offset) as u64 * 8),
                                        ],
                                    ));
                            }

                            let r = match (r, ty_size) {
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

                            lowering
                                .get_basic_block()
                                .instructions
                                .push(Instruction::new(
                                    GenericOpcode::Copy as mir::Opcode,
                                    vec![
                                        Operand::Register(
                                            mir::Register::Physical(r as PhysicalRegister),
                                            RegisterRole::Def,
                                        ),
                                        Operand::Register(
                                            mir::Register::Virtual(vreg_idx),
                                            RegisterRole::Use,
                                        ),
                                    ],
                                ));
                            last_offset = Some(offset);
                        }
                    }
                    _ => unimplemented!(),
                }
            }
        }

        if stack_offset > 0 {
            lowering.get_basic_block().instructions.insert(
                adj_stack_pos,
                Instruction::new(
                    Opcode::Sub64ri as mir::Opcode,
                    vec![
                        Operand::Register(
                            mir::Register::Physical(Register::Rsp as PhysicalRegister),
                            RegisterRole::Use,
                        ),
                        Operand::Immediate(stack_offset as u64),
                    ],
                ),
            );
        }

        lowering
            .get_basic_block()
            .instructions
            .push(Instruction::new(
                Opcode::Call64r as mir::Opcode,
                vec![
                    Operand::Register(mir::Register::Virtual(callee_vreg_idx), RegisterRole::Use),
                    Operand::Immediate(stack_offset as u64),
                ],
            ));

        if stack_offset > 0 {
            lowering
                .get_basic_block()
                .instructions
                .push(Instruction::new(
                    Opcode::Add64ri as mir::Opcode,
                    vec![
                        Operand::Register(
                            mir::Register::Physical(Register::Rsp as PhysicalRegister),
                            RegisterRole::Use,
                        ),
                        Operand::Immediate(stack_offset as u64),
                    ],
                ));
        }

        if let Some((mut vreg_indices, ty)) = ret {
            let mut registers = vec![Register::Rdx, Register::Rax];
            let mut offsets = lowering.ty_to_offsets[&ty].clone();

            for class in self.ty_class(lowering.abi, lowering.ty_storage, ty) {
                match class {
                    ClassKind::Memory => {
                        let base = lowering.create_vreg(lowering.ty_storage.ptr_ty);
                        let address_mode = AddressMode {
                            base: Base::Register(mir::Register::Physical(
                                Register::Rdi as PhysicalRegister,
                            )),
                            index: None,
                            scale: 1,
                            displacement: None,
                        };
                        let mut operands = vec![mir::Operand::Register(
                            mir::Register::Virtual(base),
                            RegisterRole::Def,
                        )];

                        address_mode.write(&mut operands, 1);
                        lowering
                            .get_basic_block()
                            .instructions
                            .push(mir::Instruction::new(
                                Opcode::Lea64 as mir::Opcode,
                                operands,
                            ));

                        for (vreg_idx, offset) in std::mem::take(&mut vreg_indices)
                            .into_iter()
                            .zip(std::mem::take(&mut offsets))
                        {
                            let ptr_add = lowering.create_vreg(lowering.ty_storage.ptr_ty);
                            let bb = lowering.get_basic_block();

                            bb.instructions.push(mir::Instruction::new(
                                GenericOpcode::PtrAdd as mir::Opcode,
                                vec![
                                    mir::Operand::Register(
                                        mir::Register::Virtual(ptr_add),
                                        RegisterRole::Def,
                                    ),
                                    mir::Operand::Register(
                                        mir::Register::Virtual(base),
                                        RegisterRole::Use,
                                    ),
                                    mir::Operand::Immediate(offset as u64),
                                ],
                            ));
                            bb.instructions.push(Instruction::new(
                                GenericOpcode::Load as mir::Opcode,
                                vec![
                                    Operand::Register(
                                        mir::Register::Virtual(vreg_idx),
                                        RegisterRole::Def,
                                    ),
                                    Operand::Register(
                                        mir::Register::Virtual(ptr_add),
                                        RegisterRole::Use,
                                    ),
                                ],
                            ));
                        }
                    }
                    ClassKind::Integer => {
                        let r = registers.pop().unwrap();
                        let (vreg_indices, offsets) =
                            get_vregs_for_one_reg(lowering, &mut vreg_indices, &mut offsets);
                        let mut last_offset = None;

                        for (vreg_idx, offset) in vreg_indices.into_iter().zip(offsets) {
                            let ty = lowering.mir_function.vreg_types[&vreg_idx];
                            let ty_size = lowering.abi.ty_size(lowering.ty_storage, ty);
                            let bb = lowering.get_basic_block();

                            if let Some(last_offset) = last_offset {
                                bb.instructions.push(Instruction::new(
                                    Opcode::Shr64ri as mir::Opcode,
                                    vec![
                                        Operand::Register(
                                            mir::Register::Physical(r as PhysicalRegister),
                                            RegisterRole::Use,
                                        ),
                                        Operand::Immediate((offset - last_offset) as u64 * 8),
                                    ],
                                ));
                            }

                            let r = match (r, ty_size) {
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

                            bb.instructions.push(Instruction::new(
                                GenericOpcode::Copy as mir::Opcode,
                                vec![
                                    Operand::Register(
                                        mir::Register::Virtual(vreg_idx),
                                        RegisterRole::Def,
                                    ),
                                    Operand::Register(
                                        mir::Register::Physical(r as PhysicalRegister),
                                        RegisterRole::Use,
                                    ),
                                ],
                            ));

                            last_offset = Some(offset);
                        }
                    }
                    _ => unimplemented!(),
                }
            }
        }
    }
}

fn get_vregs_for_one_reg<A: Abi>(
    lowering: &FnLowering<A>,
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
        let ty = lowering.mir_function.vreg_types[&vreg_idx];
        let ty_size = lowering.abi.ty_size(lowering.ty_storage, ty);

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

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
                        scale: None,
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

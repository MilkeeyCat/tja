use super::OperandKind;
use crate::{
    hir::ty,
    mir::{GenericOpcode, Mir, Opcode, Operand},
    targets::{
        Abi,
        amd64::address_mode::{AddressMode, Base},
    },
};

fn get_add_op(dest: OperandKind, src: OperandKind, size: usize) -> Opcode {
    (match (dest, src, size) {
        (OperandKind::Register, OperandKind::Register, 1) => super::Opcode::Add8rr,
        (OperandKind::Register, OperandKind::Memory, 1) => super::Opcode::Add8rm,
        (OperandKind::Memory, OperandKind::Register, 1) => super::Opcode::Add8mr,
        (OperandKind::Memory, OperandKind::Immediate, 1) => super::Opcode::Add8mi,
        (OperandKind::Register, OperandKind::Immediate, 1) => super::Opcode::Add8ri,

        (OperandKind::Register, OperandKind::Register, 2) => super::Opcode::Add16rr,
        (OperandKind::Register, OperandKind::Memory, 2) => super::Opcode::Add16rm,
        (OperandKind::Memory, OperandKind::Register, 2) => super::Opcode::Add16mr,
        (OperandKind::Memory, OperandKind::Immediate, 2) => super::Opcode::Add16mi,
        (OperandKind::Register, OperandKind::Immediate, 2) => super::Opcode::Add16ri,

        (OperandKind::Register, OperandKind::Register, 4) => super::Opcode::Add32rr,
        (OperandKind::Register, OperandKind::Memory, 4) => super::Opcode::Add32rm,
        (OperandKind::Memory, OperandKind::Register, 4) => super::Opcode::Add32mr,
        (OperandKind::Memory, OperandKind::Immediate, 4) => super::Opcode::Add32mi,
        (OperandKind::Register, OperandKind::Immediate, 4) => super::Opcode::Add32ri,

        (OperandKind::Register, OperandKind::Register, 8) => super::Opcode::Add64rr,
        (OperandKind::Register, OperandKind::Memory, 8) => super::Opcode::Add64rm,
        (OperandKind::Memory, OperandKind::Register, 8) => super::Opcode::Add64mr,
        (OperandKind::Memory, OperandKind::Immediate, 8) => super::Opcode::Add64mi,
        (OperandKind::Register, OperandKind::Immediate, 8) => super::Opcode::Add64ri,

        _ => unreachable!(),
    }) as Opcode
}

pub fn select_instructions<A: Abi>(mir: &mut Mir, abi: &A, ty_storage: &ty::Storage) {
    for module in &mut mir.0 {
        for func in &mut module.functions {
            for bb in &mut func.blocks {
                for instr in &mut bb.instructions {
                    match GenericOpcode::try_from(instr.opcode) {
                        Ok(opcode) => match opcode {
                            GenericOpcode::Add => {
                                let vreg_idx = &instr.operands[0].get_vreg_idx().unwrap();
                                let ty = func.vreg_types[vreg_idx];
                                let size = abi.ty_size(ty_storage, ty);

                                instr.opcode = get_add_op(
                                    (&instr.operands[0]).into(),
                                    (&instr.operands[1]).into(),
                                    size,
                                );
                                instr.tied_operands = Some((0, 1));
                            }
                            GenericOpcode::Sub => unimplemented!(),
                            GenericOpcode::Mul => unimplemented!(),
                            GenericOpcode::SDiv => unimplemented!(),
                            GenericOpcode::UDiv => unimplemented!(),
                            GenericOpcode::FrameIndex => {
                                let address_mode = AddressMode {
                                    base: match &instr.operands[1] {
                                        Operand::Frame(idx) => Base::Frame(*idx),
                                        _ => unreachable!(),
                                    },
                                    index: None,
                                    scale: None,
                                    displacement: None,
                                };

                                instr.opcode = super::Opcode::Lea64 as Opcode;
                                instr.operands.remove(1);
                                address_mode.write(&mut instr.operands, 1);
                            }
                            GenericOpcode::PtrAdd => {
                                let displacement = match &instr.operands[2] {
                                    Operand::Immediate(value) => *value,
                                    _ => unreachable!(),
                                };
                                let address_mode = AddressMode {
                                    base: match &instr.operands[1] {
                                        Operand::Register(r, _) => Base::Register(r.clone()),
                                        _ => unreachable!(),
                                    },
                                    index: None,
                                    scale: None,
                                    displacement: Some(displacement as isize),
                                };

                                instr.opcode = super::Opcode::Lea64 as Opcode;
                                instr.operands.remove(2);
                                instr.operands.remove(1);
                                address_mode.write(&mut instr.operands, 1);
                            }
                            GenericOpcode::Load => {
                                let vreg_idx = &instr.operands[0].get_vreg_idx().unwrap();
                                let ty = func.vreg_types[vreg_idx];
                                let size = abi.ty_size(ty_storage, ty);
                                let address_mode = AddressMode {
                                    base: match &instr.operands[1] {
                                        Operand::Register(r, _) => Base::Register(r.clone()),
                                        _ => unreachable!(),
                                    },
                                    index: None,
                                    scale: None,
                                    displacement: None,
                                };
                                let opcode = match size {
                                    1 => super::Opcode::Mov8rm,
                                    2 => super::Opcode::Mov16rm,
                                    4 => super::Opcode::Mov32rm,
                                    8 => super::Opcode::Mov64rm,
                                    _ => unreachable!(),
                                };

                                instr.opcode = opcode as Opcode;
                                instr.operands.remove(1);
                                address_mode.write(&mut instr.operands, 1);
                            }
                            GenericOpcode::Store => {
                                let vreg_idx = &instr.operands[0].get_vreg_idx().unwrap();
                                let ty = func.vreg_types[vreg_idx];
                                let size = abi.ty_size(ty_storage, ty);
                                let address_mode = AddressMode {
                                    base: match &instr.operands[1] {
                                        Operand::Register(r, _) => Base::Register(r.clone()),
                                        _ => unreachable!(),
                                    },
                                    index: None,
                                    scale: None,
                                    displacement: None,
                                };
                                let opcode = match size {
                                    1 => super::Opcode::Mov8mr,
                                    2 => super::Opcode::Mov16mr,
                                    4 => super::Opcode::Mov32mr,
                                    8 => super::Opcode::Mov64mr,
                                    _ => unreachable!(),
                                };

                                instr.opcode = opcode as Opcode;
                                instr.operands.remove(1);
                                address_mode.write(&mut instr.operands, 0);
                            }
                            GenericOpcode::Br => {
                                instr.opcode = super::Opcode::Jmp as Opcode;
                            }
                            GenericOpcode::Return => {
                                instr.opcode = super::Opcode::Ret as Opcode;
                            }
                            GenericOpcode::GlobalValue => match instr.operands[1] {
                                Operand::Function(idx) => {
                                    let address_mode = AddressMode {
                                        base: Base::Function(idx),
                                        index: None,
                                        scale: None,
                                        displacement: None,
                                    };

                                    instr.opcode = super::Opcode::Lea64 as Opcode;
                                    instr.operands.remove(1);
                                    address_mode.write(&mut instr.operands, 1);
                                }
                                _ => unreachable!(),
                            },
                            GenericOpcode::Copy => (), // skip copy instructions at this step

                            GenericOpcode::Num => unreachable!(),
                        },
                        Err(_) => (),
                    }
                }
            }
        }
    }
}

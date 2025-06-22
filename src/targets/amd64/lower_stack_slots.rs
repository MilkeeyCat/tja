use super::address_mode::{AddressMode, Base};
use crate::mir::{Function, Operand, PhysicalRegister, Register};
use std::collections::HashMap;

pub fn lower_stack_slots(func: &mut Function) {
    let mut locations = HashMap::new();
    let mut stack_offset: isize = 0;

    for bb in &mut func.blocks {
        for instr in &mut bb.instructions {
            loop {
                let mut address_mode = None;

                for (i, operand) in instr.operands.iter_mut().enumerate() {
                    if let Operand::Frame(idx) = operand {
                        stack_offset += func.stack_slots[idx] as isize;
                        address_mode = Some((
                            locations
                                .entry(*idx)
                                .or_insert(AddressMode {
                                    base: Base::Register(Register::Physical(
                                        super::Register::Rbp as PhysicalRegister,
                                    )),
                                    index: None,
                                    scale: None,
                                    displacement: Some(-stack_offset),
                                })
                                .clone(),
                            i,
                        ));

                        break;
                    }
                }

                match address_mode {
                    Some((address_mode, idx)) => {
                        instr.operands.remove(idx);
                        address_mode.write(&mut instr.operands, idx);
                    }
                    None => break,
                }
            }
        }
    }
}

use super::address_mode::{AddressMode, Base};
use crate::{
    mir::{Function, Operand, OperandIdx, Register},
    pass::{Context, Pass},
    targets::Target,
};
use std::collections::HashMap;

#[derive(Default)]
pub struct StackSlotsLowerer;

impl<'a, T: Target> Pass<'a, Function, T> for StackSlotsLowerer {
    fn run(&self, func: &mut Function, _ctx: &mut Context<'a, T>) {
        if func.is_declaration() {
            return;
        }

        let mut locations = HashMap::new();
        let mut stack_offset: isize = 0;

        for bb in &mut func.blocks {
            for instr in &mut bb.instructions {
                loop {
                    let mut address_mode = None;

                    for (i, operand) in instr.operands.iter_mut().enumerate() {
                        if let Operand::Frame(idx) = operand {
                            address_mode = Some((
                                locations
                                    .entry(*idx)
                                    .or_insert_with(|| {
                                        stack_offset +=
                                            func.frame_info.get_stack_object(*idx).size as isize;

                                        AddressMode {
                                            base: Base::Register(Register::Physical(
                                                super::Register::Rbp.into(),
                                            )),
                                            index: None,
                                            scale: 1,
                                            displacement: Some(-stack_offset),
                                        }
                                    })
                                    .clone(),
                                i,
                            ));

                            break;
                        }
                    }

                    match address_mode {
                        Some((address_mode, idx)) => {
                            instr.operands.remove(OperandIdx::new(idx + 3));
                            instr.operands.remove(OperandIdx::new(idx + 2));
                            instr.operands.remove(OperandIdx::new(idx + 1));
                            instr.operands.remove(idx.into());

                            address_mode.write(&mut instr.operands, idx.into());
                        }
                        None => break,
                    }
                }
            }
        }
    }
}

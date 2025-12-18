use crate::{
    mir::{Function, RegisterRole},
    pass::{Context, Pass},
    targets::Target,
};

#[derive(Default)]
pub struct TwoAddressForm;

impl<'a, T: Target> Pass<'a, Function, T> for TwoAddressForm {
    fn run(&self, func: &mut Function, _ctx: &mut Context<'a, T>) {
        if func.is_declaration() {
            return;
        }

        let mut bb_cursor = func.block_cursor_mut();

        while let Some(bb_idx) = bb_cursor.move_next() {
            let mut instr_cursor = bb_cursor.func.instr_cursor_mut(bb_idx).at_head();

            while instr_cursor.move_next().is_some() {
                let instr = instr_cursor.current_mut().unwrap();

                if let Some((lhs, rhs)) = std::mem::take(&mut instr.tied_operands) {
                    let instr = instr_cursor.current_mut().unwrap();
                    let rhs = instr.operands.remove(rhs);
                    let (reg, RegisterRole::Def) = instr.operands[lhs].expect_register() else {
                        unreachable!()
                    };

                    instr.implicit_uses.insert(reg.clone());

                    let lhs = instr.operands[lhs].clone().try_into().unwrap();
                    let instr_idx = instr_cursor.func.create_instr().copy(lhs, rhs);

                    instr_cursor.insert_before(instr_idx);
                }
            }
        }
    }
}

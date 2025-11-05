use crate::{
    mir::{
        self, Operand, OperandInfo, RegisterRole,
        pattern_match::{Pattern, PatternCtx, Value, predicates::Predicate},
    },
    targets::Target,
};

#[derive(Debug)]
pub struct Immediate(pub u64);

impl<'pred, T: Target, P> Pattern<'_, '_, '_, T, &[Operand]> for Value<'_, Immediate, P>
where
    for<'p> &'p P: IntoIterator<Item = &'p Box<dyn Predicate<Immediate>>>,
{
    fn matches(&mut self, _ctx: &PatternCtx<'_, '_, T>, operands: &[Operand]) -> bool {
        // TODO: It should also check if the operand is vreg and go to the
        // instruction which defines the vreg and check if it's COPY with
        // Operand::Immediate. If so, set the value directly.
        match &operands[0] {
            Operand::Immediate(value) => {
                let value = Immediate(*value);

                if self.check_predicates(&value) {
                    if let Some(dest) = &mut self.value {
                        dest.write(value);
                    }

                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

impl OperandInfo for Immediate {
    const LEN: usize = 1;
}

#[derive(Debug)]
pub struct Register(pub mir::Register, pub RegisterRole);

impl TryFrom<&[Operand]> for Register {
    type Error = ();

    fn try_from(value: &[Operand]) -> Result<Self, Self::Error> {
        match &value[0] {
            Operand::Register(reg, role) => Ok(Self(reg.clone(), *role)),
            _ => Err(()),
        }
    }
}

impl OperandInfo for Register {
    const LEN: usize = 1;
}

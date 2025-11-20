use crate::{
    mir::{
        self, Operand, OperandInfo, RegisterRole,
        pattern_match::{Pattern, PatternCtx, Value, predicates::Predicate},
    },
    targets::Target,
    ty::TyIdx,
};

#[derive(Debug)]
pub struct Immediate {
    pub value: u64,
    pub ty: Option<TyIdx>,
}

impl<'pred, T: Target, P> Pattern<'_, '_, '_, T, &[Operand]> for Value<'_, T, Immediate, P>
where
    for<'p> &'p P: IntoIterator<Item = &'p Box<dyn Predicate<T, Immediate>>>,
{
    fn matches(&mut self, ctx: &PatternCtx<'_, '_, T>, operands: &[Operand]) -> bool {
        // TODO: It should also check if the operand is vreg and go to the
        // instruction which defines the vreg and check if it's COPY with
        // Operand::Immediate. If so, set the value directly.
        match &operands[0] {
            Operand::Immediate(value) => {
                let value = Immediate {
                    value: *value,
                    ty: None,
                };

                if self.check_predicates(ctx, &value) {
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

impl IntoIterator for Immediate {
    type Item = Operand;
    type IntoIter = <[Self::Item; 1] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        [Operand::Immediate(self.value)].into_iter()
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

impl IntoIterator for Register {
    type Item = Operand;
    type IntoIter = <[Self::Item; 1] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        [Operand::Register(self.0, self.1)].into_iter()
    }
}

impl OperandInfo for Register {
    const LEN: usize = 1;
}

pub mod operands;
pub mod predicates;
mod uninitialized;

use crate::{
    mir::{
        Function, InstructionIdx, Opcode, Operand, OperandIdx, OperandInfo,
        pattern_match::predicates::Predicate,
    },
    pass::Context,
    targets::Target,
};
pub use uninitialized::Uninitialized;

pub struct PatternCtx<'a, 'ctx, T: Target> {
    pub func: &'a Function,
    pub ctx: &'a Context<'ctx, T>,
}

pub trait Pattern<'a, 'b, 'ctx, T: Target, I> {
    fn matches(&mut self, ctx: &'a PatternCtx<'b, 'ctx, T>, input: I) -> bool;
}

pub trait OperandPattern<'a, 'b: 'ops, 'ctx, 'ops, T: Target>:
    Pattern<'a, 'b, 'ctx, T, &'ops [Operand]>
{
    fn num(&self) -> usize;
}

pub struct Instruction<'a, 'b, 'ctx, 'ops, T: Target> {
    opcode: Opcode,
    patterns: Vec<Box<dyn OperandPattern<'a, 'b, 'ctx, 'ops, T> + 'ops>>,
}

impl<'a, 'b, 'ctx, 'ops, T: Target> Instruction<'a, 'b, 'ctx, 'ops, T> {
    pub fn new(
        opcode: Opcode,
        patterns: Vec<Box<dyn OperandPattern<'a, 'b, 'ctx, 'ops, T> + 'ops>>,
    ) -> Self {
        Self { opcode, patterns }
    }
}

impl<'a, 'b, 'ctx, 'ops, T: Target> Pattern<'a, 'b, 'ctx, T, InstructionIdx>
    for Instruction<'a, 'b, 'ctx, 'ops, T>
{
    fn matches(&mut self, ctx: &'a PatternCtx<'b, 'ctx, T>, instr_idx: InstructionIdx) -> bool {
        let instr = &ctx.func.instructions[instr_idx];

        if self.opcode != instr.opcode {
            return false;
        }

        let mut idx: OperandIdx = 0.into();

        for pat in &mut self.patterns {
            if !pat.matches(ctx, instr.operands[idx..pat.num().into()].as_raw_slice()) {
                return false;
            }

            idx += pat.num();
        }

        true
    }
}

pub struct Value<'val, O: OperandInfo, P>
where
    for<'p> &'p P: IntoIterator<Item = &'p Box<dyn Predicate<O>>>,
{
    value: Option<&'val mut Uninitialized<O>>,
    predicates: P,
}

impl<'val, O: OperandInfo, P> Value<'val, O, P>
where
    for<'p> &'p P: IntoIterator<Item = &'p Box<dyn Predicate<O>>>,
{
    pub fn new(value: Option<&'val mut Uninitialized<O>>, predicates: P) -> Self {
        Self { value, predicates }
    }

    fn check_predicates(&self, operand: &O) -> bool {
        (&self.predicates)
            .into_iter()
            .all(|pred| pred.matches(operand))
    }
}

impl<'ops, T: Target, O: OperandInfo + TryFrom<&'ops [Operand]>, P>
    Pattern<'_, '_, '_, T, &'ops [Operand]> for Value<'_, O, P>
where
    for<'p> &'p P: IntoIterator<Item = &'p Box<dyn Predicate<O>>>,
{
    fn matches(&mut self, _ctx: &PatternCtx<'_, '_, T>, operands: &'ops [Operand]) -> bool {
        match O::try_from(operands) {
            Ok(value) => {
                if self.check_predicates(&value) {
                    if let Some(dest) = &mut self.value {
                        dest.write(value);
                    }

                    true
                } else {
                    false
                }
            }
            Err(_) => false,
        }
    }
}

impl<'a, 'b: 'ops, 'ctx, 'ops, 'val, T: Target, O: OperandInfo, P>
    OperandPattern<'a, 'b, 'ctx, 'ops, T> for Value<'val, O, P>
where
    for<'p> &'p P: IntoIterator<Item = &'p Box<dyn Predicate<O>>>,
    Value<'val, O, P>: Pattern<'a, 'b, 'ctx, T, &'ops [Operand]>,
{
    fn num(&self) -> usize {
        O::LEN
    }
}

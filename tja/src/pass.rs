pub trait Pass<C, U, O> {
    fn run(&self, ctx: C, unit: U) -> O;
}

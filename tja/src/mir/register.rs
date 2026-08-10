use derive_more::From;
use index_vec::define_index_type;

define_index_type! {
    pub(super) struct VregIdx = usize;
}

pub(super) trait PhysicalRegister: std::fmt::Display {}

#[derive(Debug, Clone, Copy, From)]
pub(super) enum Register<PR: PhysicalRegister> {
    Physical(PR),
    Virtual(VregIdx),
}

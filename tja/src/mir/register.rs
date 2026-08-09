use index_vec::define_index_type;

define_index_type! {
    pub(super) struct VregIdx = usize;
}

trait PhysicalRegister: std::fmt::Display {}

enum Register<PR: PhysicalRegister> {
    Physical(PR),
    Virtual(VregIdx),
}

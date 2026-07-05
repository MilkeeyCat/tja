#[derive(Debug, PartialEq)]
pub(super) struct RegisterClass(u8);

include!(concat!(env!("OUT_DIR"), "/amd64/register_class.rs"));

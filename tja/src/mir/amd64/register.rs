#[derive(Debug, PartialEq)]
pub(super) struct Register(u8);

include!(concat!(env!("OUT_DIR"), "/amd64/register.rs"));

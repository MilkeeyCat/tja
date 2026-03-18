use crate::targets;

include!(concat!(env!("OUT_DIR"), "/amd64/register_class.rs"));

impl targets::RegisterClass for RegisterClass {}

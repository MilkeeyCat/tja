use crate::macros::usize_wrapper;

usize_wrapper! {Opcode}

include!(concat!(env!("OUT_DIR"), "/generic_opcodes.rs"));

impl TryFrom<Opcode> for GenericOpcode {
    type Error = ();

    fn try_from(value: Opcode) -> Result<Self, Self::Error> {
        if *value < Self::num() {
            Ok(unsafe { std::mem::transmute::<_, Self>(value) })
        } else {
            Err(())
        }
    }
}

impl Into<Opcode> for GenericOpcode {
    fn into(self) -> Opcode {
        Opcode(self as usize)
    }
}

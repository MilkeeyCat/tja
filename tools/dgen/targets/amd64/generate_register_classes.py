from io import TextIOWrapper

from dgen.base.constraints import REGISTER_CLASSES


def generate_register_classes(buf: TextIOWrapper):
    buf.write(
        """#[repr(usize)]
pub enum RegisterClass {"""
    )

    for reg_class in REGISTER_CLASSES:
        buf.write(
            f"""
    {reg_class.name},"""
        )

    buf.write("\n}")

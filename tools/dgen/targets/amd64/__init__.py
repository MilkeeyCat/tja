from pathlib import Path

from .registers import *
from .register_classes import *
from .generate_registers import generate_registers
from .generate_register_info import generate_register_info
from .generate_register_classes import generate_register_classes


def generate(out: str):
    root = Path(out) / "amd64"

    if not root.is_dir():
        root.mkdir(parents=True)

    generate_registers(open(root / "register.rs", "w"))
    generate_register_classes(open(root / "register_classes.rs", "w"))
    generate_register_info(open(root / "register_info.rs", "w"))

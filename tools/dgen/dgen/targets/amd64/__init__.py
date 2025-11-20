from pathlib import Path

from dgen.base.generate_instruction_selector import generate_instruction_selector

from .registers import *
from .register_classes import *
from .instructions import *
from .isel_patterns import *
from .generate_registers import generate_registers
from .generate_register_info import generate_register_info
from .generate_register_classes import generate_register_classes
from .generate_asm_printer import generate_asm_printer
from .generate_instruction_opcodes import generate_instruction_opcodes


def generate(out: str):
    root = Path(out) / "amd64"

    if not root.is_dir():
        root.mkdir(parents=True)

    generate_registers(open(root / "register.rs", "w"))
    generate_register_classes(open(root / "register_classes.rs", "w"))
    generate_register_info(open(root / "register_info.rs", "w"))
    generate_asm_printer(open(root / "asm_printer.rs", "w"))
    generate_instruction_opcodes(open(root / "opcode.rs", "w"))
    generate_instruction_selector(open(root / "instruction_selector.rs", "w"))

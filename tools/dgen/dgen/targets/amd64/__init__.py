from pathlib import Path

from .registers import *
from .register_classes import *
from .instructions import *
from .generate_register import generate_register
from .generate_register_class import generate_register_class


def generate(out: str):
    root = Path(out) / "amd64"

    if not root.is_dir():
        root.mkdir(parents=True)

    generate_register(open(root / "register.rs", "w"))
    generate_register_class(open(root / "register_class.rs", "w"))

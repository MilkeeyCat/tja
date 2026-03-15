from pathlib import Path
from typing import cast

import importlib
import argparse

from dgen.base.generate_generic_instruction import generate_generic_instruction

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-t")
    parser.add_argument("-o")
    args = parser.parse_args()
    target = cast(str, args.t)
    out = cast(str, args.o)

    generate_generic_instruction(open(Path(out) / "generic_instruction.rs", "w"))

    module = importlib.import_module(f".targets.{target}", "dgen")
    module.generate(out)

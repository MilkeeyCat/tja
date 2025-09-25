from typing import cast

import importlib
import argparse

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-t")
    parser.add_argument("-o")
    args = parser.parse_args()
    target = cast(str, args.t)
    out = cast(str, args.o)

    module = importlib.import_module(f".targets.{target}", "dgen")
    module.generate(out)

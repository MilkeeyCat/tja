import importlib
import argparse
from typing import cast

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    _ = parser.add_argument("-t")
    _ = parser.add_argument("-o")
    args = parser.parse_args()
    target = cast(str, args.t)
    out = cast(str, args.o)

    _ = importlib.import_module(f".targets.{target}", "dgen")

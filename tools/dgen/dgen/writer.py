from io import TextIOWrapper


class Writer:
    buf: TextIOWrapper
    level: int

    def __init__(self, buf: TextIOWrapper):
        self.buf = buf
        self.level = 0

    def indent(self):
        self.level += 1

    def dedent(self):
        self.level -= 1

    def write(self, s: str):
        self.buf.write(" " * self.level * 4 + s)

    def writeln(self, s: str):
        self.write(s + "\n")

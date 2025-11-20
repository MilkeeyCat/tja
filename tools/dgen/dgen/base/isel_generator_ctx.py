from dgen.writer import Writer


class IselGeneratorCtx:
    writer: Writer

    def __init__(self, writer: Writer):
        self.writer = writer

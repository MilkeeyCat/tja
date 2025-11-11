class Type:
    type_idx: int

    def __init__(self, type_idx: int):
        self.type_idx = type_idx


I8 = Type(1)
I16 = Type(2)
I32 = Type(3)
I64 = Type(4)

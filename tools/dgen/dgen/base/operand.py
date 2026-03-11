class Operand:
    """
    gMIR instruction operand, represented by a rust type. Any valid rust type
    can be an operand, be it a concrete one(`BlockIdx`) or a generic one(`R`).
    """

    type_name: str
    is_generic: bool

    def __init__(self, type_name: str, is_generic: bool = False):
        self.type_name = type_name
        self.is_generic = is_generic

    def __str__(self) -> str:
        return self.type_name

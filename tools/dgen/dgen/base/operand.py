class Operand:
    """
    gMIR instruction operand, represented by a rust type. Any valid rust type
    can be an operand, be it a concrete one(`BlockIdx`) or a generic one(`F`).
    """

    type_name: str

    def __init__(self, type_name: str):
        self.type_name = type_name

    def __str__(self) -> str:
        return self.type_name

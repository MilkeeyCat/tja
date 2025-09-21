from io import TextIOWrapper

from dgen.base.instruction import INSTRUCTIONS, TokenType, parse_asm_string


def generate_asm_printer(buf: TextIOWrapper):
    buf.write("impl<'a, T: Target, W: Write> AsmPrinter<'a, T, W> {\n")
    buf.write(
        "\tfn emit_instr(&mut self, module: &Module, instr: &Instruction) -> std::fmt::Result {\n"
    )
    buf.write("\t\tlet operands = instr.operands.as_raw_slice();\n\n")
    buf.write("\t\tmatch Opcode::from(instr.opcode) {\n")

    for instr in INSTRUCTIONS:
        buf.write(f"\t\t\tOpcode::{instr.name} => {{\n")

        for type, token in parse_asm_string(instr.asm):
            buf.write("\t\t\t\t")

            match type:
                case TokenType.STRING:
                    buf.write(f'write!(self.buf, "{token}");\n')
                case TokenType.IDENT:
                    (offset, operand) = instr.get_operand(token)

                    buf.write(
                        f"self.{operand.emit_method}(module, &operands[{offset}..{offset+operand.operands_len}])?;\n"
                    )

        buf.write("\t\t\t}\n")

    # TODO: remove the line below once generated Opcode enum is used
    buf.write(f"\t\t\t_ => unreachable!(),\n")

    buf.write("\t\t}\n\n")
    buf.write("\t\tOk(())\n")
    buf.write("\t}\n")
    buf.write("}\n")

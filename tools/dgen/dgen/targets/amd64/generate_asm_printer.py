from io import TextIOWrapper

from dgen.base.instruction import TARGET_INSTRUCTIONS, TokenType, parse_asm_string
from dgen.writer import Writer


def generate_asm_printer(buf: TextIOWrapper):
    writer = Writer(buf)

    writer.writeln("impl<'a, T: Target, W: Write> AsmPrinter<'a, T, W> {")
    writer.indent()
    writer.writeln(
        "fn emit_instr(&mut self, module: &Module, fn_idx: FunctionIdx, instr: &Instruction) -> std::fmt::Result {"
    )
    writer.indent()
    writer.writeln("let operands = instr.operands.as_raw_slice();")
    buf.write("\n")
    writer.writeln("match Opcode::from(instr.opcode) {")
    writer.indent()

    for instr in TARGET_INSTRUCTIONS:
        writer.writeln(f"Opcode::{instr.name} => {{")
        writer.indent()

        for type, token in parse_asm_string(instr.asm):
            match type:
                case TokenType.STRING:
                    writer.writeln(f'write!(self.buf, "{token}")?;')
                case TokenType.IDENT:
                    operand = instr.get_operand(token)
                    offset = (
                        " + ".join(
                            [
                                f"{operand.struct}::LEN"
                                for operand in instr.get_operands_before(token)
                            ]
                        )
                        or "0"
                    )

                    writer.writeln(f"let offset = {offset};")
                    writer.writeln(
                        f"self.{operand.emit_method}(module, fn_idx, &operands[offset..offset + {operand.struct}::LEN])?;"
                    )

        writer.dedent()
        writer.writeln("}")

    # TODO: remove the line below once generated Opcode enum is used
    writer.writeln(f"_ => unreachable!(),")
    writer.dedent()

    writer.writeln("}")
    buf.write("\n")
    writer.writeln("Ok(())")
    writer.dedent()
    writer.writeln("}")
    writer.dedent()
    writer.writeln("}")

from .register_classes import GPR16, GPR32, GPR64, GPR8
from .registers import AH, AL, AX, DX, EAX, EDX, RAX, RBP, RDX
from .instruction import Instruction
from .operand import r, w, rw, implicit
from .operands import *

# ==============================================================================
# MOV
# ==============================================================================


Instruction("mov_rr8", "mov", [w(GPR8), r(GPR8)])
Instruction("mov_mr8", "mov", [MEM8, w(GPR8)])

Instruction("mov_rr16", "mov", [w(GPR16), r(GPR16)])
Instruction("mov_mr16", "mov", [MEM16, w(GPR16)])

Instruction("mov_rr32", "mov", [w(GPR32), r(GPR32)])
Instruction("mov_mr32", "mov", [MEM32, w(GPR32)])

Instruction("mov_rr64", "mov", [w(GPR64), r(GPR64)])
Instruction("mov_mr64", "mov", [MEM64, w(GPR64)])

Instruction("mov_rm8", "mov", [w(GPR8), MEM8])
Instruction("mov_rm16", "mov", [w(GPR16), MEM16])
Instruction("mov_rm32", "mov", [w(GPR32), MEM32])
Instruction("mov_rm64", "mov", [w(GPR64), MEM64])

Instruction("mov_ri8", "mov", [w(GPR8), IMM8])
Instruction("mov_mi8", "mov", [MEM8, IMM8])

Instruction("mov_ri16", "mov", [w(GPR16), IMM16])
Instruction("mov_mi16", "mov", [MEM16, IMM16])

Instruction("mov_ri32", "mov", [w(GPR32), IMM32])
Instruction("mov_mi32", "mov", [MEM32, IMM32])

Instruction("mov_ri64", "mov", [w(GPR64), IMM64])


# ==============================================================================
# MOVSX
# ==============================================================================


Instruction("movsx_r16r8", "movsx", [w(GPR16), r(GPR8)])
Instruction("movsx_r16m8", "movsx", [w(GPR16), MEM8])

Instruction("movsx_r32r8", "movsx", [w(GPR32), r(GPR8)])
Instruction("movsx_r32m8", "movsx", [w(GPR32), MEM8])

Instruction("movsx_r64r8", "movsx", [w(GPR64), r(GPR8)])
Instruction("movsx_r64m8", "movsx", [w(GPR64), MEM8])

Instruction("movsx_r32r16", "movsx", [w(GPR32), r(GPR16)])
Instruction("movsx_r32m16", "movsx", [w(GPR32), MEM16])

Instruction("movsx_r64r16", "movsx", [w(GPR64), r(GPR16)])
Instruction("movsx_r64m16", "movsx", [w(GPR64), MEM16])


# ==============================================================================
# ADD
# ==============================================================================


Instruction("add_ri8", "add", [rw(GPR8), IMM8])
Instruction("add_mi8", "add", [MEM8, IMM8])

Instruction("add_ri16", "add", [rw(GPR16), IMM16])
Instruction("add_mi16", "add", [MEM16, IMM16])

Instruction("add_ri32", "add", [rw(GPR32), IMM32])
Instruction("add_mi32", "add", [MEM32, IMM32])

Instruction("add_ri64", "add", [rw(GPR64), IMM64])
Instruction("add_mi64", "add", [MEM64, IMM64])

Instruction("add_rr8", "add", [rw(GPR8), r(GPR8)])
Instruction("add_mr8", "add", [MEM8, r(GPR8)])

Instruction("add_rr16", "add", [rw(GPR16), r(GPR16)])
Instruction("add_mr16", "add", [MEM16, r(GPR16)])

Instruction("add_rr32", "add", [rw(GPR32), r(GPR32)])
Instruction("add_mr32", "add", [MEM32, r(GPR32)])

Instruction("add_rr64", "add", [rw(GPR64), r(GPR64)])
Instruction("add_mr64", "add", [MEM64, r(GPR64)])

Instruction("add_r64i32", "add", [rw(GPR64), IMM32])


# ==============================================================================
# SUB
# ==============================================================================


Instruction("sub_ri8", "sub", [rw(GPR8), IMM8])
Instruction("sub_mi8", "sub", [MEM8, IMM8])

Instruction("sub_ri16", "sub", [rw(GPR16), IMM16])
Instruction("sub_mi16", "sub", [MEM16, IMM16])

Instruction("sub_ri32", "sub", [rw(GPR32), IMM32])
Instruction("sub_mi32", "sub", [MEM32, IMM32])

Instruction("sub_ri64", "sub", [rw(GPR64), IMM64])
Instruction("sub_mi64", "sub", [MEM64, IMM64])

Instruction("sub_rr8", "sub", [rw(GPR8), r(GPR8)])
Instruction("sub_mr8", "sub", [MEM8, r(GPR8)])

Instruction("sub_rr16", "sub", [rw(GPR16), r(GPR16)])
Instruction("sub_mr16", "sub", [MEM16, r(GPR16)])

Instruction("sub_rr32", "sub", [rw(GPR32), r(GPR32)])
Instruction("sub_mr32", "sub", [MEM32, r(GPR32)])

Instruction("sub_rr64", "sub", [rw(GPR64), r(GPR64)])
Instruction("sub_mr64", "sub", [MEM64, r(GPR64)])

Instruction("sub_r64i32", "sub", [rw(GPR64), IMM32])


# ==============================================================================
# IMUL
# ==============================================================================


Instruction("imul_r8", "imul", [implicit(w(AX)), implicit(r(AL)), r(GPR8)])
Instruction("imul_r16", "imul", [implicit(rw(AX)), implicit(w(DX)), r(GPR16)])
Instruction("imul_r32", "imul", [implicit(rw(EAX)), implicit(w(EDX)), r(GPR32)])
Instruction("imul_r64", "imul", [implicit(rw(RAX)), implicit(w(RDX)), r(GPR64)])


# ==============================================================================
# IDIV
# ==============================================================================


Instruction(
    "idiv_r8", "idiv", [implicit(w(AL)), implicit(w(AH)), implicit(r(AX)), r(GPR8)]
)
Instruction("idiv_r16", "idiv", [implicit(rw(AX)), implicit(rw(DX)), r(GPR16)])
Instruction("idiv_r32", "idiv", [implicit(rw(EAX)), implicit(rw(EDX)), r(GPR32)])
Instruction("idiv_r64", "idiv", [implicit(rw(RAX)), implicit(rw(RDX)), r(GPR64)])


# ==============================================================================
# CMP
# ==============================================================================


Instruction("cmp_rr8", "cmp", [r(GPR8), r(GPR8)])
Instruction("cmp_mr8", "cmp", [MEM8, r(GPR8)])

Instruction("cmp_rr16", "cmp", [r(GPR16), r(GPR16)])
Instruction("cmp_mr16", "cmp", [MEM16, r(GPR16)])

Instruction("cmp_rr32", "cmp", [r(GPR32), r(GPR32)])
Instruction("cmp_mr32", "cmp", [MEM32, r(GPR32)])

Instruction("cmp_rr64", "cmp", [r(GPR64), r(GPR64)])
Instruction("cmp_mr64", "cmp", [MEM64, r(GPR64)])

Instruction("cmp_ri8", "cmp", [r(GPR8), IMM8])


# ==============================================================================
# XOR
# ==============================================================================


Instruction("xor_rr8", "xor", [rw(GPR8), r(GPR8)])
Instruction("xor_rr16", "xor", [rw(GPR16), r(GPR16)])
Instruction("xor_rr32", "xor", [rw(GPR32), r(GPR32)])
Instruction("xor_rr64", "xor", [rw(GPR64), r(GPR64)])


# ==============================================================================
# LEA
# ==============================================================================


Instruction("lea_r32m", "lea", [w(GPR64), ADDR])


# ==============================================================================
# SHL
# ==============================================================================


Instruction("shl_r64i8", "shl", [rw(GPR64), IMM8])


# ==============================================================================
# SHR
# ==============================================================================


Instruction("shr_r64i8", "shr", [rw(GPR64), IMM8])


# ==============================================================================
# PUSH
# ==============================================================================


Instruction("push_r64", "push", [r(GPR64)])


# ==============================================================================
# POP
# ==============================================================================


Instruction("pop_r64", "pop", [w(GPR64)])


# ==============================================================================
# CALL
# ==============================================================================


Instruction("call_r64", "call", [r(GPR64)])


# ==============================================================================
# JMP
# ==============================================================================


Instruction("jmp", "jmp", [BLOCK])


# ==============================================================================
# JCC
# ==============================================================================


# TODO: add more variants
Instruction("ja", "ja", [BLOCK])


# ==============================================================================
# SETCC
# ==============================================================================


# TODO: add more variants
Instruction("seta_r8", "seta", [w(GPR8)])
Instruction("seta_m8", "seta", [MEM8])


# ==============================================================================
# LEAVE
# ==============================================================================


Instruction("leave_64", "leave", [implicit(rw(RBP))])


# ==============================================================================
# RET
# ==============================================================================


Instruction("ret", "ret", [])


# ==============================================================================
# CWD/CDQ/CQO
# ==============================================================================


Instruction("cwd", "cwd", [implicit(w(DX)), implicit(r(AX))])
Instruction("cdq", "cdq", [implicit(w(EDX)), implicit(r(EAX))])
Instruction("cqo", "cqo", [implicit(w(RDX)), implicit(r(RAX))])

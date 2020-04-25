================================
MARCAL Ternary RISC Architecture
================================

:Author: Nagy Tibor
:Contact: xnagytibor@gmail.com

.. contents::

Introduction
============

Architecture Goals
------------------

The goal of this project to provide a platform for experimenting with ternary
computing. The top priority is simplicity, defining an easy-to-implement yet
flexible RISC architecture heavily inspired by existing binary architectures,
and developing an accompanying software environment.

The project doesn't aim to be compatible with any existing binary software or
data formats.

Balanced Ternary
----------------

Ternary Integer Types
---------------------

+--------------+-----------+-----------+-----------+
| Name         | No. trits | Minimum   | Maximum   |
+==============+===========+===========+===========+
| Word         |        12 |   -265720 |    265720 |
+--------------+-----------+-----------+-----------+
| Half-word    |         6 |      -364 |       364 |
+--------------+-----------+-----------+-----------+
| Quarter-word |         3 |       -13 |        13 |
+--------------+-----------+-----------+-----------+
| Trit         |         1 |        -1 |         1 |
+--------------+-----------+-----------+-----------+

Memory
======

TODO: Address space 12 trits
TODO: 729 pages, 729 words per page
Zero-page mapped as ROM,

Registers
=========

TODO: All registers are 12-trit in size
TODO: 27 registers, U0-U13, S0-S13
TODO: Zero = U0 = S0, Program counter = S1
TODO: No dedicated stack register, ABI-defined
TODO: PC initialized to 0 on boot, other registers are in an undefined state on boot

System Registers
----------------

+-----+-----------+--------------------------------+
| No. | Name      | Description                    |
+=====+===========+================================+
|   0 | S0 / ZERO | Dedicated read-only zero       |
|     |           | register.                      |
+-----+-----------+--------------------------------+
|   1 | S1 / PC   | Program counter holding the    |
|     |           | next instruction's address.    |
+-----+-----------+--------------------------------+
|   2 | S2        | Registers S2-S13 are reserved  |
+-----+-----------+ for system software use.       |
|   3 | S3        |                                |
+-----+-----------+                                |
|   4 | S4        |                                |
+-----+-----------+                                |
|   5 | S5        |                                |
+-----+-----------+                                |
|   6 | S6        |                                |
+-----+-----------+                                |
|   7 | S7        |                                |
+-----+-----------+                                |
|   8 | S8        |                                |
+-----+-----------+                                |
|   9 | S9        |                                |
+-----+-----------+                                |
|  10 | S10       |                                |
+-----+-----------+                                |
|  11 | S11       |                                |
+-----+-----------+                                |
|  12 | S12       |                                |
+-----+-----------+                                |
|  13 | S13       |                                |
+-----+-----------+--------------------------------+

User Registers
--------------

+-----+-----------+--------------------------------+
| No. | Name      | Description                    |
+=====+===========+================================+
|   0 | U0 / ZERO | Alias of the S0 dedicated zero |
|     |           | register.                      |
+-----+-----------+--------------------------------+
|  -1 | U1        | Registers U1-U13 are general   |
+-----+-----------+ purpose registers with         |
|  -2 | U2        | no usage restrictions.         |
+-----+-----------+                                |
|  -3 | U3        |                                |
+-----+-----------+                                |
|  -4 | U4        |                                |
+-----+-----------+                                |
|  -5 | U5        |                                |
+-----+-----------+                                |
|  -6 | U6        |                                |
+-----+-----------+                                |
|  -7 | U7        |                                |
+-----+-----------+                                |
|  -8 | U8        |                                |
+-----+-----------+                                |
|  -9 | U9        |                                |
+-----+-----------+                                |
| -10 | U10       |                                |
+-----+-----------+                                |
| -11 | U11       |                                |
+-----+-----------+                                |
| -12 | U12       |                                |
+-----+-----------+                                |
| -13 | U13       |                                |
+-----+-----------+--------------------------------+

Instruction Set
===============

Instruction Encoding Format
---------------------------

+------------------+----+----+----+----+----+----+----+----+----+----+----+----+
| Format           | 11 | 10 |  9 |  8 |  7 |  6 |  5 |  4 |  3 |  2 |  1 |  0 |
+==================+====+====+====+====+====+====+====+====+====+====+====+====+
| RGTR             | Opcode       | RD           | RA           | RB           |
+------------------+--------------+--------------+--------------+--------------+
| IMM3             | Opcode       | RD           | RA           | Imm          |
+------------------+--------------+--------------+--------------+--------------+
| IMM6             | Opcode       | RD           | Imm                         |
+------------------+--------------+--------------+-----------------------------+

TODO:
  When registers or immediate arguments not present, use implicit 0/S0 arguments.

Base Instruction Set
--------------------

+-----+--------+-------------------+-------------------------------------------+
| Op. | Format | Instruction       | Description                               |
+=====+========+===================+===========================================+
| -13 |        |                   | Undefined instruction.                    |
+-----+--------+-------------------+-------------------------------------------+
| -12 |        |                   | Undefined instruction.                    |
+-----+--------+-------------------+-------------------------------------------+
| -11 |        |                   | Undefined instruction.                    |
+-----+--------+-------------------+-------------------------------------------+
| -10 |        |                   | Undefined instruction.                    |
+-----+--------+-------------------+-------------------------------------------+
|  -9 |        |                   | Undefined instruction.                    |
+-----+--------+-------------------+-------------------------------------------+
|  -8 |        |                   | Undefined instruction.                    |
+-----+--------+-------------------+-------------------------------------------+
|  -7 |        |                   | Undefined instruction.                    |
+-----+--------+-------------------+-------------------------------------------+
|  -6 |        |                   | Undefined instruction.                    |
+-----+--------+-------------------+-------------------------------------------+
|  -5 | RGTR   | ROLR RD RA RB     | Rotate register left.                     |
|     |        |                   |                                           |
|     |        |                   | RD := RA <rot< RB                         |
+-----+--------+-------------------+-------------------------------------------+
|  -4 | IMM3   | SHLQ RD RA 12     | Shift register left with immediate.       |
|     |        |                   |                                           |
|     |        |                   | RD := RA << Imm                           |
+-----+--------+-------------------+-------------------------------------------+
|  -3 | RGTR   | SHLR RD RA RB     | Shift register left with register.        |
|     |        |                   |                                           |
|     |        |                   | RD := RA << RB                            |
+-----+--------+-------------------+-------------------------------------------+
|  -2 | RGTR   | NEGR RD RA        | Negation.                                 |
|     |        |                   |                                           |
|     |        |                   | RD := -RA                                 |
+-----+--------+-------------------+-------------------------------------------+
|  -1 | RGTR   | DYAD RD RA RB     | Dyadic function.                          |
|     |        |                   |                                           |
|     |        |                   | See: `The dyadic function`_ subsection    |
+-----+--------+-------------------+-------------------------------------------+
|   0 | RGTR   | ADDR RD RA RB     | Add register to register.                 |
|     |        |                   |                                           |
|     |        |                   | RD := RA + RB                             |
+-----+--------+-------------------+-------------------------------------------+
|   1 | IMM3   | ADDQ RD RA 12     | Add quarter-word to register.             |
|     |        |                   |                                           |
|     |        |                   | RD := RA + Imm                            |
+-----+--------+-------------------+-------------------------------------------+
|   2 | IMM6   | ADDH RD 123       | Add half-word to register.                |
|     |        |                   |                                           |
|     |        |                   | RD := RD + Imm                            |
+-----+--------+-------------------+-------------------------------------------+
|   3 | IMM6   | LDLH RD 123       | Load half-word to the lower half of       |
|     |        |                   | register RD, clear the higher half with   |
|     |        |                   | zeroes.                                   |
|     |        |                   |                                           |
|     |        |                   | RD := Imm                                 |
+-----+--------+-------------------+-------------------------------------------+
|   4 | IMM6   | LDHH RD 123       | Load half-word to higher half of          |
|     |        |                   | register RD, clear the lower half with    |
|     |        |                   | zeroes.                                   |
|     |        |                   |                                           |
|     |        |                   | RD := Imm << 6                            |
+-----+--------+-------------------+-------------------------------------------+
|   5 | RGTR   | LDMR RD RA RB     | Load memory to register.                  |
|     |        |                   |                                           |
|     |        |                   | RD := Memory[RA + RB]                     |
+-----+--------+-------------------+-------------------------------------------+
|   6 | RGTR   | STMR RD RA RB     | Store register to memory.                 |
|     |        |                   |                                           |
|     |        |                   | Memory[RA + RB] := RD                     |
+-----+--------+-------------------+-------------------------------------------+
|   7 | RGTR   | BREQ RD RA RB     | Branch to RD when RA = RB                 |
+-----+--------+-------------------+-------------------------------------------+
|   8 | RGTR   | BRNE RD RA RB     | Branch to RD when RA <> RB                |
+-----+--------+-------------------+-------------------------------------------+
|   9 | RGTR   | BRLT RD RA RB     | Branch to RD when RA < RB                 |
+-----+--------+-------------------+-------------------------------------------+
|  10 | RGTR   | BRLE RD RA RB     | Branch to RD when RA <= RB                |
+-----+--------+-------------------+-------------------------------------------+
|  11 | RGTR   | PUSH SP RA        | Push RA to stack SP                       |
+-----+--------+-------------------+-------------------------------------------+
|  12 | RGTR   | PULL SP RA        | Pull RA from stack SP                     |
+-----+--------+-------------------+-------------------------------------------+
|  13 | RGTR   | CALL SP RA        | Call subroutine RA using stack SP         |
+-----+--------+-------------------+-------------------------------------------+

Pseudoinstructions
------------------

+-------------------+-------------------+--------------------------------------+
| Pseudoinstruction | Expansion         | Description                          |
+===================+===================+======================================+
| NOOP              || ADDR S0 S0 S0    | No operaton                          |
+-------------------+-------------------+--------------------------------------+
| MOVR RD RA        || ADDR RD RA S0    | Move register to register            |
+-------------------+-------------------+--------------------------------------+
| RETURN SP         || PULL SP PC       | Return from subroutine               |
+-------------------+-------------------+--------------------------------------+
| BRGT RD RA RB     || BRLE RD RB RA    | Branch to RD when RA > RB            |
+-------------------+-------------------+--------------------------------------+
| BRGE RD RA RB     || BRLT RD RB RA    | Branch to RD when RA >= RB           |
+-------------------+-------------------+--------------------------------------+
| LDW RD -264992    || LDHH RD -364     | Load immediate word to RD            |
|                   || ADDH RD 364      |                                      |
|                   |                   | The PC register cannot be used as    |
|                   |                   | the RD argument. Unconditional jumps |
|                   |                   | can be implemented with this however |
|                   |                   | the jump addresses first must be     |
|                   |                   | loaded into a non-special purpose    |
|                   |                   | register, then transferred to PC     |
|                   |                   | with a single instruction.           |
+-------------------+-------------------+--------------------------------------+
| LDM RD 212686     || LDHH RD 292      | Load word from address to            |
|                   || ADDH RD -182     | register RD                          |
|                   || LDMR RD RD S0    |                                      |
+-------------------+-------------------+--------------------------------------+
| SUBR RD RA RB     || NEGR RD RB       | RD := RA - RB                        |
|                   || ADDR RD RD RA    |                                      |
+-------------------+-------------------+--------------------------------------+
| STACK SP RD -1    || LDLH RD -1       | Load a word from stack SP            |
|                   || LDMR RD SP RD    | to register RD from the              |
|                   |                   | specified index                      |
+-------------------+-------------------+--------------------------------------+
| MASK RD RA 4      || SHLQ RD RA 8     | Masking the N leftmost trits.        |
|                   || SHLQ RD RD -8    | The shift amount for N trits is      |
|                   |                   | (12 - N) and -(12 - N)               |
|                   |                   |                                      |
|                   |                   | DYAD could also be used for this,    |
|                   |                   | however it's less effective with     |
|                   |                   | register clobbering.                 |
+-------------------+-------------------+--------------------------------------+

The dyadic function
-------------------

DYAD RD RA RB

The DYAD instruction can be used to perform any of the 19683 possible
ternary logic functions by specifying the operations' truth table in the RD
register. Upon execution the truth table specified in RD is overwritten by
the result of the operation.


Truth table representation
..........................

+---------+--------------+
|         |      RB      |
|         +----+----+----+
|         || - || 0 || + |
+----+----+----+----+----+
|    || - || A || B || C |
|    +----+----+----+----+
| RA || 0 || D || E || F |
|    +----+----+----+----+
|    || + || G || H || I |
+----+----+----+----+----+

Truth table value := [000IHGFEDCBA]

The top three trits of the truth table value are always 0.

Common dyadic functions
.......................

+-------------------+-------------------+--------------------------------------+
| Truth table value | Usage             | Description                          |
+===================+===================+======================================+
|              5792 || LDHH RD 8        | Logical AND                          |
|                   || ADDH RD -40      |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|             -5792 || LDHH RD -8       | Logical NAND                         |
|                   || ADDH RD 40       |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|              9728 || LDHH RD 13       | Logical OR                           |
|                   || ADDH RD 251      |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|             -9728 || LDHH RD -13      | Logical NOR                          |
|                   || ADDH RD -251     |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|             -5824 || LDHH RD -8       | Logical XOR                          |
|                   || ADDH RD 8        |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|              5824 || LDHH RD 8        | Logical XNOR / Multiplication        |
|                   || ADDH RD -8       |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|              3445 || LDHH RD 5        | Equality                             |
|                   || ADDH RD -200     |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|             -3445 || LDHH RD -5       | Inequality                           |
|                   || ADDH RD 200      |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|             -9331 || LDHH RD -13      | Less than                            |
|                   || ADDH RD 146      |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|              3955 || LDHH RD 5        | Less than or equals                  |
|                   || ADDH RD 310      |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|             -3955 || LDHH RD -5       | Greater than                         |
|                   || ADDH RD -310     |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|              9331 || LDHH RD 13       | Greater than or equals               |
|                   || ADDH RD -146     |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|              6560 || LDHH RD 9        | Consensus                            |
|                   || ADDH RD -1       |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|              8960 || LDHH RD 12       | Accept anything                      |
|                   || ADDH RD 212      |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|              6088 || LDHH RD 8        | Kleene implication                   |
|                   || ADDH RD 256      |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+
|              6169 || LDHH RD 8        | Lukasiewicz implication              |
|                   || ADDH RD 337      |                                      |
|                   || DYAD RD RA RB    |                                      |
+-------------------+-------------------+--------------------------------------+

Common monadic functions
........................

Monadic functions can also be performed with DYAD by setting the second operand
register to ZERO.

+-------------------+-------------------+--------------------------------------+
| Truth table value | Usage             | Description                          |
+===================+===================+======================================+
|             -2184 || LDHH RD -3       | Inversion                            |
|                   || ADDH RD 3        |                                      |
|                   || DYAD RD RA ZERO  |                                      |
+-------------------+-------------------+--------------------------------------+
|             -2265 || LDHH RD -3       | Negative threshold inversion         |
|                   || ADDH RD -78      |                                      |
|                   || DYAD RD RA ZERO  |                                      |
+-------------------+-------------------+--------------------------------------+
|             -2103 || LDHH RD -3       | Positive threshold inversion         |
|                   || ADDH RD 84       |                                      |
|                   || DYAD RD RA ZERO  |                                      |
+-------------------+-------------------+--------------------------------------+
|             -2106 || LDHH RD -3       | Increment                            |
|                   || ADDH RD 81       |                                      |
|                   || DYAD RD RA ZERO  |                                      |
+-------------------+-------------------+--------------------------------------+
|               -78 || LDHH RD 0        | Decrement                            |
|                   || ADDH RD -78      |                                      |
|                   || DYAD RD RA ZERO  |                                      |
+-------------------+-------------------+--------------------------------------+
|             -2265 || LDHH RD -3       | Is false?                            |
|                   || ADDH RD -78      |                                      |
|                   || DYAD RD RA ZERO  |                                      |
+-------------------+-------------------+--------------------------------------+
|             -2109 || LDHH RD -3       | Is unknown?                          |
|                   || ADDH RD 78       |                                      |
|                   || DYAD RD RA ZERO  |                                      |
+-------------------+-------------------+--------------------------------------+
|              2103 || LDHH RD 3        | Is true?                             |
|                   || ADDH RD -84      |                                      |
|                   || DYAD RD RA ZERO  |                                      |
+-------------------+-------------------+--------------------------------------+
|                -3 || LDHH RD 0        | Clamp down                           |
|                   || ADDH RD -3       |                                      |
|                   || DYAD RD RA ZERO  |                                      |
+-------------------+-------------------+--------------------------------------+
|              2187 || LDHH RD 3        | Clamp up                             |
|                   || ADDH RD 0        |                                      |
|                   || DYAD RD RA ZERO  |                                      |
+-------------------+-------------------+--------------------------------------+


Shift/Rotate:
  Operates in a +12 .. -12 range
  Outside it has an undefined behaviour

TODO:
  Integer overflow -> undefined

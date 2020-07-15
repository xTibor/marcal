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

The architecture has a 12-trit address space, divided into 729 pages. Each of
these pages hold 729 words of data. The only addressable data unit is the
12-trit machine word.

Pages are indexed from Page -364 to Page 364, the words inside these pages are
also indexed from -364 to 364. When memory addresses are separated into two
half-words, the higher half-word corresponds to the page index, the lower one
corresponds to word offset inside the page.

There are two pages with fixed purpose:

+---------+-----------------------+--------------------------------------------+
|         | Memory address        |                                            |
|         +-------+-------+-------+ Description                                |
|         | Low   | Mid   | High  |                                            |
+=========+=======+=======+=======+============================================+
| Page 0  |  -364 |     0 |   364 | The program counter is initialized to 0    |
|         |       |       |       | on startup, therefore this page contains   |
|         |       |       |       | the initialization code of the system.     |
+---------+-------+-------+-------+--------------------------------------------+
| Page -1 | -1093 |  -729 |  -365 | See: `Memory mapping information page`_    |
|         |       |       |       | subsection                                 |
+---------+-------+-------+-------+--------------------------------------------+

Memory mapping information page
-------------------------------

Page -1 contains the memory mapping of the system. Each word inside this page
holds the mapping entry of pages with the same offset. The MMIP itself is mapped
as read-only.

MMIP is used by the system software to discover available RAM pages and
memory mapped I/O devices.

+-----+--------------------+---------------------------------------------------+
| No. | Mapping type       | Description                                       |
+=====+====================+===================================================+
|   0 | Unmapped           |                                                   |
+-----+--------------------+---------------------------------------------------+
|   1 | ROM                |                                                   |
+-----+--------------------+---------------------------------------------------+
|   2 | RAM                |                                                   |
+-----+--------------------+---------------------------------------------------+
|   3 | Serial I/O device  | Terminals, network devices, etc.                  |
+-----+--------------------+---------------------------------------------------+

Registers
=========

TODO: All registers are 12-trit in size
TODO: 27 registers, U0-U13, S0-S13
TODO: Zero = U0 = S0, Program counter = S1
TODO: No dedicated stack register, ABI-defined
TODO: PC initialized to 0 on boot, other registers are in an undefined state on boot

System Registers
----------------

+-----+-----------+------------------------------------------------------------+
| No. | Name      | Description                                                |
+=====+===========+============================================================+
|   0 | S0 / ZERO | Dedicated read-only zero register.                         |
+-----+-----------+------------------------------------------------------------+
|   1 | S1 / PC   | Program counter holding the next instruction's address.    |
+-----+-----------+------------------------------------------------------------+
|   2 | S2 / IHA  | Interrupt handler address.                                 |
+-----+-----------+------------------------------------------------------------+
|   3 | S3 / IRA  | Interrupt return address.                                  |
|     |           |                                                            |
|     |           | Volatile. Must only be set by:                             |
|     |           |                                                            |
|     |           | - SWIR / RETI,                                             |
|     |           | - interrupt handler (for preemptive multitasking).         |
+-----+-----------+------------------------------------------------------------+
|   4 | S4 / INUM | Interrupt number.                                          |
|     |           |                                                            |
|     |           | Volatile. Must only be set by:                             |
|     |           |                                                            |
|     |           | - SWIR / RETI,                                             |
|     |           | - interrupt handler.                                       |
+-----+-----------+------------------------------------------------------------+
|   5 | S5        | Registers S5-S13 are reserved for system use.              |
+-----+-----------+                                                            |
|   6 | S6        |                                                            |
+-----+-----------+                                                            |
|   7 | S7        |                                                            |
+-----+-----------+                                                            |
|   8 | S8        |                                                            |
+-----+-----------+                                                            |
|   9 | S9        |                                                            |
+-----+-----------+                                                            |
|  10 | S10       |                                                            |
+-----+-----------+                                                            |
|  11 | S11       |                                                            |
+-----+-----------+                                                            |
|  12 | S12       |                                                            |
+-----+-----------+                                                            |
|  13 | S13       |                                                            |
+-----+-----------+------------------------------------------------------------+

User Registers
--------------

+-----+-----------+------------------------------------------------------------+
| No. | Name      | Description                                                |
+=====+===========+============================================================+
|   0 | U0 / ZERO | Alias of the S0 dedicated zero register.                   |
+-----+-----------+------------------------------------------------------------+
|  -1 | U1        | Registers U1-U13 are general purpose registers with        |
+-----+-----------+ no usage restrictions.                                     |
|  -2 | U2        |                                                            |
+-----+-----------+                                                            |
|  -3 | U3        |                                                            |
+-----+-----------+                                                            |
|  -4 | U4        |                                                            |
+-----+-----------+                                                            |
|  -5 | U5        |                                                            |
+-----+-----------+                                                            |
|  -6 | U6        |                                                            |
+-----+-----------+                                                            |
|  -7 | U7        |                                                            |
+-----+-----------+                                                            |
|  -8 | U8        |                                                            |
+-----+-----------+                                                            |
|  -9 | U9        |                                                            |
+-----+-----------+                                                            |
| -10 | U10       |                                                            |
+-----+-----------+                                                            |
| -11 | U11       |                                                            |
+-----+-----------+                                                            |
| -12 | U12       |                                                            |
+-----+-----------+                                                            |
| -13 | U13       |                                                            |
+-----+-----------+------------------------------------------------------------+

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
| -13 |        |                   | Reserved for extended instruction         |
|     |        |                   | encodings.                                |
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
|  -7 | RGTR   | SWIR RD           | Raise a software interrupt.               |
|     |        |                   |                                           |
|     |        |                   | | ISF := 1                                |
|     |        |                   | | IRA := PC                               |
|     |        |                   | | IRNUM := RD                             |
|     |        |                   | | PC := IRH                               |
|     |        |                   |                                           |
|     |        |                   | See: `Interrupts`_ subsection             |
+-----+--------+-------------------+-------------------------------------------+
|  -6 | RGTR   | RETI              | Return from interrupt.                    |
|     |        |                   |                                           |
|     |        |                   | Moves IRA to PC and clears the interrupt  |
|     |        |                   | status flag.                              |
|     |        |                   |                                           |
|     |        |                   | | ISF := 0                                |
|     |        |                   | | PC := IRA                               |
|     |        |                   |                                           |
|     |        |                   | See: `Interrupts`_ subsection             |
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

LDHH RD >VALUE
ADDH RD <VALUE
DYAD RD RA RB

+----------------+-------------+-----------------------------------------------+
| Register value | Truth table | Description                                   |
+================+=============+===============================================+
|           5792 || - - -      | Logical AND                                   |
|                || - 0 0      |                                               |
|                || - 0 +      |                                               |
+----------------+-------------+-----------------------------------------------+
|          -5792 || + + +      | Logical NAND                                  |
|                || + 0 0      |                                               |
|                || + 0 -      |                                               |
+----------------+-------------+-----------------------------------------------+
|           9728 || - 0 +      | Logical OR                                    |
|                || 0 0 +      |                                               |
|                || + + +      |                                               |
+----------------+-------------+-----------------------------------------------+
|          -9728 || + 0 -      | Logical NOR                                   |
|                || 0 0 -      |                                               |
|                || - - -      |                                               |
+----------------+-------------+-----------------------------------------------+
|          -5824 || - 0 +      | Logical XOR                                   |
|                || 0 0 0      |                                               |
|                || + 0 -      |                                               |
+----------------+-------------+-----------------------------------------------+
|           5824 || + 0 -      | Logical XNOR / Multiplication                 |
|                || 0 0 0      |                                               |
|                || - 0 +      |                                               |
+----------------+-------------+-----------------------------------------------+
|           3445 || + - -      | Equality                                      |
|                || - + -      |                                               |
|                || - - +      |                                               |
+----------------+-------------+-----------------------------------------------+
|          -3445 || - + +      | Inequality                                    |
|                || + - +      |                                               |
|                || + + -      |                                               |
+----------------+-------------+-----------------------------------------------+
|          -9331 || - + +      | Less than                                     |
|                || - - +      |                                               |
|                || - - -      |                                               |
+----------------+-------------+-----------------------------------------------+
|           3955 || + + +      | Less than or equals                           |
|                || - + +      |                                               |
|                || - - +      |                                               |
+----------------+-------------+-----------------------------------------------+
|          -3955 || - - -      | Greater than                                  |
|                || + - -      |                                               |
|                || + + -      |                                               |
+----------------+-------------+-----------------------------------------------+
|           9331 || + - -      | Greater than or equals                        |
|                || + + -      |                                               |
|                || + + +      |                                               |
+----------------+-------------+-----------------------------------------------+
|           6560 || - 0 0      | Consensus                                     |
|                || 0 0 0      |                                               |
|                || 0 0 +      |                                               |
+----------------+-------------+-----------------------------------------------+
|           8960 || - - 0      | Accept anything                               |
|                || - 0 +      |                                               |
|                || 0 + +      |                                               |
+----------------+-------------+-----------------------------------------------+
|           6088 || + + +      | Kleene implication                            |
|                || 0 0 +      |                                               |
|                || - 0 +      |                                               |
+----------------+-------------+-----------------------------------------------+
|           6169 || + + +      | Lukasiewicz implication                       |
|                || 0 + +      |                                               |
|                || - 0 +      |                                               |
+----------------+-------------+-----------------------------------------------+

Common monadic functions
........................

Monadic functions can also be performed with DYAD by setting the second operand
register to ZERO.

LDHH RD >VALUE
ADDH RD <VALUE
DYAD RD RA ZERO

+----------------+-------------+-----------------------------------------------+
| Register value | Truth table | Description                                   |
+================+=============+===============================================+
|          -2184 || 0 + 0      | Inversion                                     |
|                || 0 0 0      |                                               |
|                || 0 - 0      |                                               |
+----------------+-------------+-----------------------------------------------+
|          -2265 || 0 + 0      | Negative threshold inversion                  |
|                || 0 - 0      |                                               |
|                || 0 - 0      |                                               |
+----------------+-------------+-----------------------------------------------+
|          -2103 || 0 + 0      | Positive threshold inversion                  |
|                || 0 + 0      |                                               |
|                || 0 - 0      |                                               |
+----------------+-------------+-----------------------------------------------+
|          -2106 || 0 0 0      | Increment                                     |
|                || 0 + 0      |                                               |
|                || 0 - 0      |                                               |
+----------------+-------------+-----------------------------------------------+
|            -78 || 0 + 0      | Decrement                                     |
|                || 0 - 0      |                                               |
|                || 0 0 0      |                                               |
+----------------+-------------+-----------------------------------------------+
|          -2265 || 0 + 0      | Is false?                                     |
|                || 0 - 0      |                                               |
|                || 0 - 0      |                                               |
+----------------+-------------+-----------------------------------------------+
|          -2109 || 0 - 0      | Is unknown?                                   |
|                || 0 + 0      |                                               |
|                || 0 - 0      |                                               |
+----------------+-------------+-----------------------------------------------+
|           2103 || 0 - 0      | Is true?                                      |
|                || 0 - 0      |                                               |
|                || 0 + 0      |                                               |
+----------------+-------------+-----------------------------------------------+
|             -3 || 0 - 0      | Clamp down                                    |
|                || 0 0 0      |                                               |
|                || 0 0 0      |                                               |
+----------------+-------------+-----------------------------------------------+
|           2187 || 0 0 0      | Clamp up                                      |
|                || 0 0 0      |                                               |
|                || 0 + 0      |                                               |
+----------------+-------------+-----------------------------------------------+


Shift/Rotate:
  Operates in a +12 .. -12 range
  Outside it has an undefined behaviour

TODO:
  Integer overflow -> undefined


Interrupts
==========

TODO: IHA initialized to 0
TODO: System starts with the Reset interrupt (Number 0)
TODO: Enters user code on reset by setting IRA and calling RETI

TODO: Interrupt status flag (0/1)
TODO: What happens when SWIR called inside an interrupt handler (fail)
TODO: What happens when RETI called outside an interrupt handler (fail)

TODO: Explain IRA/INUM volatility (hardware interrupts could clobber them any time)

TODO: Explain how IRA/INUM set/cleared on SWIR/RETI

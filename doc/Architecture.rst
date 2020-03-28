================================
MARCAL Ternary RISC Architecture
================================

:Author: Nagy Tibor
:Contact: xnagytibor@gmail.com

.. contents::

Introduction
============

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
TODO:
    RGTR:
      000 000 000 +--
      OP  RD  RA  RB
    IMM3:
      000 000 000 000
      OP  RD  RA  IMM
    IMM6:
      000 000 000 000
      OP  RD  IMM-IMM

Instruction Set Listing
-----------------------

Pseudoinstructions
------------------

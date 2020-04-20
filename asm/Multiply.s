                                              LONG MULTIPLICATION EXAMPLE

INITARGS   LDLH U1 -123                       OPERAND A
           LDLH U2 -234                       OPERAND B
           LDLH U3 0                          RESULT

INITMULT   LDLH U4 0                          COUNTER
           LDHH U5 8                          MULTIPLICATION TRUTH TABLE
           ADDH U5 -8
           LDLH U6 0                          TEMPORARY 1
           LDLH U7 0                          TEMPORARY 2

LOOPSTART  NEGR U6 U4 ZERO                    GET THE NTH BIT OF OPERAND B
           LSHR U6 U2 U6
           LSHI U6 U6 11
           LSHI U6 U6 -11
EXPAND     ADDQ U7 PC 4                       EXPAND THE RIGHTMOST TRIT
           BRLE U7 U6 ZERO
           LDHH U6 364
           ADDH U6 364
           ADDH PC 4
           ADDQ U7 PC 7
           BRLE U7 ZERO U6
           LDHH U6 -364
           ADDH U6 -364
SHIFTADD   ADDR U7 U5 ZERO                    LOAD TRUTH TABLE
           DYAD U7 U1 U6                      MULTIPLY OPERAND A WITH EXPANDED
           LSHR U7 U7 U4                      SHIFT LEFT BY COUNTER VALUE
           ADDR U3 U3 U7                      ADD TO RESULT
LOOPNEXT   ADDH U4 1                          STEP THE COUNTER
           LDLH U7 12                         MAX COUNTER VALUE
           LDLH U6 -21                        SET BRANCH TARGET TO LOOPSTART
           ADDR U6 PC U6
           BRLT U6 U4 U7                      BRANCH TO LOOPSTART

HALT       UD13 ZERO ZERO ZERO

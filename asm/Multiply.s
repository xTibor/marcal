                                              LONG MULTIPLICATION EXAMPLE

INITARGS   LDLH U1 -123                       OPERAND A
           LDLH U2 -234                       OPERAND B
           LDLH U3 0                          RESULT

INITMULT   LDLH U4 11                         COUNTER
           LDHH U5 8                          MULTIPLICATION TRUTH TABLE
           ADDH U5 -8
           LDLH U6 0                          TEMPORARY 1
           LDLH U7 0                          TEMPORARY 2
           LDHH U8 364                        PATTERN WITH EVERY TRIT SET
           ADDH U8 364
           ADDQ U9 PC 10                      BRANCH TARGET 1
           ADDQ U10 PC 1                      BRANCH TARGET 2
           ADDQ U11 PC 13                     BRANCH TARGET 3

LOOPSTART  NEGR U6 U4 ZERO                    GET THE NTH BIT OF OPERAND B
           LSHR U6 U2 U6
           LSHI U6 U6 11
           LSHI U6 U6 -11

           BREQ U11 U6 ZERO
           BRLT U9 U6 ZERO
EXPPLUS    ADDR U6 U8 ZERO                    EXPAND THE RIGHTMOST TRIT AS PLUS
           ADDH PC 1
EXPMINUS   NEGR U6 U8 ZERO                    EXPAND THE RIGHTMOST TRIT AS MINUS

SHIFTADD   ADDR U7 U5 ZERO                    LOAD TRUTH TABLE
           DYAD U7 U1 U6                      MULTIPLY OPERAND A WITH EXPANDED
           LSHR U7 U7 U4                      SHIFT LEFT BY COUNTER VALUE
           ADDR U3 U3 U7                      ADD TO RESULT

LOOPNEXT   ADDH U4 -1                         STEP THE COUNTER
           BRLE U10 ZERO U4                   BRANCH TO LOOPSTART

           UD12 U3 ZERO ZERO
HALT       UD13 ZERO ZERO ZERO

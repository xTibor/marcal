                                              LONG MULTIPLICATION
                                              EXAMPLE PROGRAM


START      LDHH S9 1                          SETUP STACK POINTER
           LDLH U1 0                          OPERAND A
           LDLH U2 0                          OPERAND B
           LDLH U12 100                       ITERATION COUNT
LOOP       ADDQ U13 PC 'MULTIPLY
           CALL S9 U13
           UD12 U3
           ADDH U1 1
           ADDH U2 1
           ADDQ U13 PC 'LOOP
           BRLE U13 U1 U12
HALT       UD13


                                              LONG MULTIPLICATION ROUTINE
                                              U1  OPERAND A
                                              U2  OPERAND B
                                              U3  RESULT
MULTIPLY   PUSH S9 U4
           PUSH S9 U5
           PUSH S9 U6
           PUSH S9 U7
           PUSH S9 U8
           PUSH S9 U9
           PUSH S9 U10
           PUSH S9 U11
           LDLH U3 0                          RESULT
           LDLH U4 11                         COUNTER
           LDHH U5 >5824                      MULTIPLICATION TRUTH TABLE
           ADDH U5 <5824
           LDLH U6 0                          TEMPORARY 1
           LDLH U7 0                          TEMPORARY 2
           LDHH U8 >265720                    TRIT PATTERN WITH EVERY TRIT SET
           ADDH U8 <265720
           ADDQ U9 PC 'EXPMINUS               BRANCH TARGET 1
           ADDQ U10 PC 'LOOPSTART             BRANCH TARGET 2
           ADDQ U11 PC 'LOOPNEXT              BRANCH TARGET 3
LOOPSTART  NEGR U6 U4                         GET THE NTH BIT OF OPERAND B
           SHLR U6 U2 U6
           SHLQ U6 U6 11
           SHLQ U6 U6 -11
           BREQ U11 U6 ZERO
           BRLT U9 U6 ZERO
EXPPLUS    ADDR U6 U8 ZERO                    EXPAND THE RIGHTMOST TRIT AS PLUS
           ADDH PC 1
EXPMINUS   NEGR U6 U8                         EXPAND THE RIGHTMOST TRIT AS MINUS
SHIFTADD   ADDR U7 U5 ZERO                    LOAD TRUTH TABLE
           DYAD U7 U1 U6                      MULTIPLY OPERAND A WITH EXPANDED
           SHLR U7 U7 U4                      SHIFT LEFT BY COUNTER VALUE
           ADDR U3 U3 U7                      ADD TO RESULT
LOOPNEXT   ADDH U4 -1                         STEP THE COUNTER
           BRLE U10 ZERO U4                   BRANCH TO LOOPSTART
           PULL S9 U11
           PULL S9 U10
           PULL S9 U9
           PULL S9 U8
           PULL S9 U7
           PULL S9 U6
           PULL S9 U5
           PULL S9 U4
           PULL S9 PC

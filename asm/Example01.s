                                              LOOPS
                                              EXAMPLE PROGRAM

START      LDHH U11 >10000                    MAX VALUE
           ADDH U11 <10000
           LDLH U1 0                          COUNTER 1
           LDLH U2 0                          COUNTER 2
           ADDQ U12 PC 'LOOP                  BRANCH TARGET
LOOP       ADDH U1 1                          STEP COUNTER 1
           BRNE U12 U11 U1                    COUNTER 1 NOT EQUALS MAX VALUE
           LDHH U1 >-10000                    SET COUNTER 1 TO -10000
           ADDH U1 <-10000
           ADDH U2 1                          STEP COUNTER 2
           ADDH PC 'LOOP

START      LDHH U11 14                        SET MAX VALUE TO 10000
           ADDH U11 -206
           LDLH U1 0                          SET COUNTER 1 TO 0
           LDLH U2 0                          SET COUNTER 2 TO 0
           ADDH U1 1                          STEP COUNTER 1
           ADDQ U12 PC 4                      SET BRANCH TARGET
           BRNE U12 U11 U1                    COUNTER 1 NOT EQUALS MAX VALUE
           LDHH U1 -14                        SET COUNTER 1 TO -10000
           ADDH U1 206
           ADDH U2 1                          STEP COUNTER 2
           ADDH PC -7                         LOOP

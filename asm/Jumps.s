                                              EXAMPLE PROGRAM

                                              U1   CURRENT TEST NUMBER
                                              U13  TARGET ADDRESS


TAGRET1    LDLH U1 1
           LDHH U13 >TARGET2                  JUMP TO ABSOLUTE ADDRESS
           ADDH U13 <TARGET2                  UNCONDITIONAL, JUMP FROM REGISTER
           ADDR PC U13 ZERO
           UD13
           UD13
           UD13

TARGET2    LDLH U1 2
           ADDH PC 'TARGET3                   JUMP TO 6 TRIT RELATIVE OFFSET
           UD13                               UNCONDITIONAL, IMMEDIATE JUMP
           UD13
           UD13

TARGET3    LDLH U1 3
           ADDQ PC PC 'TARGET4                JUMP TO 3 TRIT RELATIVE OFFSET
           UD13                               UNCONDITIONAL, IMMEDIATE JUMP
           UD13
           UD13

TARGET4    LDLH U1 4
           ADDQ U13 PC 'TARGET5               JUMP TO 3 TRIT RELATIVE OFFSET
           ADDR PC U13 ZERO                   UNCONDITIONAL, JUMP FROM REGISTER
           UD13
           UD13
           UD13

TARGET5    LDLH U1 5
           LDLH U13 "TARGET6                  JUMP TO 6 TRIT RELATIVE OFFSET
           ADDR U13 U13 PC                    UNCONDITIONAL, JUMP FROM REGISTER
           ADDR PC U13 ZERO
           UD13
           UD13
           UD13

TARGET6    LDLH U1 6
           UD13
           UD13
           UD13

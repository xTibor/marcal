                                              SHIFT AND ROTATE
                                              EXAMPLE PROGRAM

START      LDHH U1 >163520
           ADDH U1 <163520
           LDLH U2 -12
           LDLH U3 12
           ADDQ U4 PC 'LOOP
LOOP       SHLR U10 U1 U2
           ROLR U11 U1 U2
           UD12 U10
           UD12 U11
           ADDH U2 1
           BRLE U4 U2 U3
           UD13

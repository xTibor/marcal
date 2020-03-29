unit Arch;

interface

type
  TOpcode = (
    ocReserved13           = -13, { <??> - Undefined     }
    ocReserved12           = -12, { <??> - Undefined     }
    ocReserved11           = -11, { <??> - Undefined     }
    ocReserved10           = -10, { <??> - Undefined     }
    ocReserved9            =  -9, { <??> - Undefined     }
    ocReserved8            =  -8, { <??> - Undefined     }
    ocReserved7            =  -7, { <??> - Undefined     }
    ocReserved6            =  -6, { <??> - Undefined     }
    ocReserved5            =  -5, { <??> - Undefined     }
    ocRotate               =  -4, { RGTR - ROTR RD RA RB }
    ocShift                =  -3, { RGTR - LSHR RD RA RB }
    ocNegation             =  -2, { RGTR - NEGR RD RA    }
    ocDyadicFunction       =  -1, { RGTR - DYAD RD RA RB }
    ocAddRegister          =   0, { RGTR - ADDR RD RA RB }
    ocAddImmediateQuarter  =   1, { IMM3 - ADDQ RD RA 12 }
    ocAddImmediateHalf     =   2, { IMM6 - ADDH RD 123   }
    ocLoadLowImmediate     =   3, { IMM6 - LDLH RD 123   }
    ocLoadHighImmediate    =   4, { IMM6 - LDHH RD 123   }
    ocLoadMemory           =   5, { RGTR - LDMR RD RA RB }
    ocStoreMemory          =   6, { RGTR - STMR RD RA RB }
    ocBranchEquals         =   7, { RGTR - BREQ RD RA RB }
    ocBranchNotEquals      =   8, { RGTR - BRNE RD RA RB }
    ocBranchLessThan       =   9, { RGTR - BRLT RD RA RB }
    ocBranchLessEqualsThan =  10, { RGTR - BRLE RD RA RB }
    ocPush                 =  11, { RGTR - PUSH SP RA    }
    ocPull                 =  12, { RGTR - PULL SP RA    }
    ocCall                 =  13  { RGTR - CALL SP RA    }
  );

  TRegister = (
    regUser13         = -13,
    regUser12         = -12,
    regUser11         = -11,
    regUser10         = -10,
    regUser9          =  -9,
    regUser8          =  -8,
    regUser7          =  -7,
    regUser6          =  -6,
    regUser5          =  -5,
    regUser4          =  -4,
    regUser3          =  -3,
    regUser2          =  -2,
    regUser1          =  -1,
    regZero           =   0,
    regProgramCounter =   1,
    regSystem2        =   2,
    regSystem3        =   3,
    regSystem4        =   4,
    regSystem5        =   5,
    regSystem6        =   6,
    regSystem7        =   7,
    regSystem8        =   8,
    regSystem9        =   9,
    regSystem10       =  10,
    regSystem11       =  11,
    regSystem12       =  12,
    regSystem13       =  13
  );

implementation

end.

unit Arch;

interface

type
  TInstructionFormat = (
    ifRegister,   { RGTR }
    ifImmediate3, { IMM3 }
    ifImmediate6  { IMM6 }
  );

  TInstructionOpcode = (
    iocUndefined13          = -13, { <??> - Undefined     }
    iocUndefined12          = -12, { <??> - Undefined     }
    iocUndefined11          = -11, { <??> - Undefined     }
    iocUndefined10          = -10, { <??> - Undefined     }
    iocUndefined9           =  -9, { <??> - Undefined     }
    iocUndefined8           =  -8, { <??> - Undefined     }
    iocUndefined7           =  -7, { <??> - Undefined     }
    iocUndefined6           =  -6, { <??> - Undefined     }
    iocRotate               =  -5, { RGTR - ROTR RD RA RB }
    iocShiftImmediate       =  -4, { IMM3 - LSHI RD RA 12 }
    iocShiftRegister        =  -3, { RGTR - LSHR RD RA RB }
    iocNegation             =  -2, { RGTR - NEGR RD RA    }
    iocDyadicFunction       =  -1, { RGTR - DYAD RD RA RB }
    iocAddRegister          =   0, { RGTR - ADDR RD RA RB }
    iocAddImmediateQuarter  =   1, { IMM3 - ADDQ RD RA 12 }
    iocAddImmediateHalf     =   2, { IMM6 - ADDH RD 123   }
    iocLoadLowImmediate     =   3, { IMM6 - LDLH RD 123   }
    iocLoadHighImmediate    =   4, { IMM6 - LDHH RD 123   }
    iocLoadMemory           =   5, { RGTR - LDMR RD RA RB }
    iocStoreMemory          =   6, { RGTR - STMR RD RA RB }
    iocBranchEquals         =   7, { RGTR - BREQ RD RA RB }
    iocBranchNotEquals      =   8, { RGTR - BRNE RD RA RB }
    iocBranchLessThan       =   9, { RGTR - BRLT RD RA RB }
    iocBranchLessEqualsThan =  10, { RGTR - BRLE RD RA RB }
    iocPush                 =  11, { RGTR - PUSH SP RA    }
    iocPull                 =  12, { RGTR - PULL SP RA    }
    iocCall                 =  13  { RGTR - CALL SP RA    }
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

const
  CInstructionMnemonics: array[TInstructionOpcode] of String = (
    'UD13', 'UD12', 'UD11',
    'UD10', 'UD09', 'UD08',
    'UD07', 'UD06', 'ROTR',
    'LSHI', 'LSHR', 'NEGR',
    'DYAD', 'ADDR', 'ADDQ',
    'ADDH', 'LDLH', 'LDHH',
    'LDMR', 'STMR', 'BREQ',
    'BRNE', 'BRLT', 'BRLE',
    'PUSH', 'PULL', 'CALL'
  );

  CInstructionFormats: array[TInstructionOpcode] of TInstructionFormat = (
    ifRegister,   ifRegister,   ifRegister,
    ifRegister,   ifRegister,   ifRegister,
    ifRegister,   ifRegister,   ifRegister,
    ifImmediate3, ifRegister,   ifRegister,
    ifRegister,   ifRegister,   ifImmediate3,
    ifImmediate6, ifImmediate6, ifImmediate6,
    ifRegister,   ifRegister,   ifRegister,
    ifRegister,   ifRegister,   ifRegister,
    ifRegister,   ifRegister,   ifRegister
  );

  CRegisterNames: array[TRegister, 0..2] of String = (
    { Name,  Alias, Alias }
    ('U13',  '',    ''  ),
    ('U12',  '',    ''  ),
    ('U11',  '',    ''  ),
    ('U10',  '',    ''  ),
    ('U9',   '',    ''  ),
    ('U8',   '',    ''  ),
    ('U7',   '',    ''  ),
    ('U6',   '',    ''  ),
    ('U5',   '',    ''  ),
    ('U4',   '',    ''  ),
    ('U3',   '',    ''  ),
    ('U2',   '',    ''  ),
    ('U1',   '',    ''  ),
    ('ZERO', 'U0',  'S0'),
    ('PC',   'S1',  ''  ),
    ('S2',   '',    ''  ),
    ('S3',   '',    ''  ),
    ('S4',   '',    ''  ),
    ('S5',   '',    ''  ),
    ('S6',   '',    ''  ),
    ('S7',   '',    ''  ),
    ('S8',   '',    ''  ),
    ('S9',   '',    ''  ),
    ('S10',  '',    ''  ),
    ('S11',  '',    ''  ),
    ('S12',  '',    ''  ),
    ('S13',  '',    ''  )
  );

function RegisterToStr(ARegister: TRegister): String;

implementation

function RegisterToStr(ARegister: TRegister): String;
begin
  RegisterToStr := CRegisterNames[ARegister, Low(CRegisterNames[ARegister])];
end;

end.

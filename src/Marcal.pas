{
  http://homepage.divms.uiowa.edu/~jones/ternary/logic.shtml
  https://rosettacode.org/wiki/Balanced_ternary
  https://www.youtube.com/watch?v=EbJMtJq20NY
  https://www.freepascal.org/docs-html/rtl/system/index-5.html
  https://www.freepascal.org/docs-html/prog/progse2.html#progsu65.html
}

{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}

program Marcal;

uses
  Test, Trit, Tryte;

{ MARCAL }
{
  000 000 000 +--
  OP  RD  RA  RB

  000 000 000 000
  OP  RD  IMM-IMM

  When registers or immediate arguments not present, use implicit 0 arguments.

  R0 - Zero
  R1 - Program counter

  Integer overflow -> crash
}

type
  TOpcode = (
    ocReserved13           = -13, { <?> }
    ocReserved12           = -12, { <?> }
    ocReserved11           = -11, { <?> }
    ocReserved10           = -10, { <?> }
    ocReserved9            =  -9, { <?> }
    ocReserved8            =  -8, { <?> }
    ocReserved7            =  -7, { <?> }
    ocReserved6            =  -6, { <?> }
    ocReserved5            =  -5, { <?> }
    ocReserved4            =  -4, { <?> }
    ocReserved3            =  -3, { <?> }
    ocReserved2            =  -2, { <?> }
    ocReserved1            =  -1, { <?> }
    ocAddRegister          =   0, { REG } { ADDR RD, RA, RB }
    ocAddImmediate         =   1, { IMM } { ADDI RD, 123    }
    ocLoadLowImmediate     =   2, { IMM } { LDLI RD, 123    }
    ocLoadHighImmediate    =   3, { IMM } { LDHI RD, 123    }
    ocLoadMemory           =   4, { REG } { LDMR RD, RA, RB }
    ocStoreMemory          =   5, { REG } { STMR RD, RA, RB }
    ocBranchEquals         =   6, { REG } { BREQ RD, RA, RB }
    ocBranchNotEquals      =   7, { REG } { BRNE RD, RA, RB }
    ocBranchLessThan       =   8, { REG } { BRLT RD, RA, RB }
    ocBranchLessEqualsThan =   9, { REG } { BRLE RD, RA, RB }
    ocPush                 =  10, { REG } { PSHR SP, RA     }
    ocPop                  =  11, { REG } { POPR SP, RA     }
    ocCall                 =  12, { REG } { CALL SP, RA     }
    ocReserved             =  13  { <?> }
    { Synthesized opcodes ------------------------------------------- }
    { NoOperation             } { NOOP             => ADDR R0, R0, R0 }
    { Return                  } { RTRN             => POPR SP, PC     }
    { BranchGreaterThan       } { BRGT RD, RA, RB  => BRLE RD, RB, RA }
    { BranchGreaterEqualsThan } { BRGE RD, RA, RB  => BRLT RD, RB, RA }
    { LoadImmediate           } { LDI RD, -264992  => LDHI RD, -364   }
                                {                     ADDI RD, 364    }
    { LoadMemory              } { LDM RD, 212686   => LDHI RD, 292    }
                                {                     ADDI RD, -182   }
                                {                     LDMR RD, RD, R0 }
    { --------------------------------------------------------------- }
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

  TExecutionContext = record
    Registers: array[TRegister] of TLongTryte;
    Memory: array[TLongTryte] of TLongTryte;
    Halt: Boolean;
  end;

procedure InitContext(var AContext: TExecutionContext);
var
  LRegister: TRegister;
  LIndex: LongInt;
begin
  for LRegister := Low(AContext.Registers) to High(AContext.Registers) do
    AContext.Registers[LRegister] := 0;

  for LIndex := Low(AContext.Memory) to High(AContext.Memory) do
    AContext.Memory[LIndex] := 0;

  AContext.Halt := false;
end;

procedure PrintContext(var AContext: TExecutionContext);
var
  LIndex: LongInt;
  LValue: TLongTryte;
begin
  LValue := AContext.Registers[regZero];
  WriteLn(LongTryteToStr(LValue), ' (', LValue:7, ') ');

  for LIndex := 1 to 13 do begin
    LValue := AContext.Registers[TRegister(+LIndex)];
    Write(LongTryteToStr(LValue), ' (', LValue:7, ') ');

    LValue := AContext.Registers[TRegister(-LIndex)];
    Write(LongTryteToStr(LValue), ' (', LValue:7, ') ');

    WriteLn();
  end;
  WriteLn();
end;

procedure ExecuteContext(var AContext: TExecutionContext);
var
  LProgramCounter: TLongTryte;

  LShortSplice: TShortSplice;
  LOpcode: TOpcode;
  LRegA: TRegister;
  LRegB: TRegister;
  LRegD: TRegister;

  LHalfSplice: THalfSplice;
  LImmediate: THalfTryte;
begin
  with AContext do begin
    LProgramCounter := Memory[Registers[regProgramCounter]];
    LShortSplice := LongTryteShortSplice(LProgramCounter);
    LHalfSplice  := LongTryteHalfSplice(LProgramCounter);

    LOpcode    := TOpcode(LShortSplice[3]);
    LRegD      := TRegister(LShortSplice[2]);
    LRegA      := TRegister(LShortSplice[1]);
    LRegB      := TRegister(LShortSplice[0]);
    LImmediate := LHalfSplice[0];

    WriteLn(LOpcode, ' ', LRegD, ' ', LRegA, ' ', LRegB);
    WriteLn(LongTryteToStr(LImmediate), ' (', LImmediate:7, ') ');
    WriteLn();

    Registers[regProgramCounter] += 1;

    case LOpcode of
      ocLoadLowImmediate:
        if LRegD <> regZero then
          Registers[LRegD] := LImmediate;
      ocLoadHighImmediate:
        if LRegD <> regZero then
          Registers[LRegD] := LImmediate * 729; { << 6 }
      ocAddImmediate:
        if LRegD <> regZero then
          Registers[LRegD] += LImmediate;
      ocAddRegister:
        if LRegD <> regZero then
          Registers[LRegD] := Registers[LRegA] + Registers[LRegB];
      ocLoadMemory:
        if LRegD <> regZero then
          Registers[LRegD] := Memory[Registers[LRegA] + Registers[LRegB]];
      ocStoreMemory:
        Memory[Registers[LRegA] + Registers[LRegB]] := Registers[LRegD];
      ocBranchEquals:
        if Registers[LRegA] = Registers[LRegB] then
          Registers[regProgramCounter] := Registers[LRegD];
      ocBranchNotEquals:
        if Registers[LRegA] <> Registers[LRegB] then
          Registers[regProgramCounter] := Registers[LRegD];
      ocBranchLessThan:
        if Registers[LRegA] < Registers[LRegB] then
          Registers[regProgramCounter] := Registers[LRegD];
      ocBranchLessEqualsThan:
        if Registers[LRegA] <= Registers[LRegB] then
          Registers[regProgramCounter] := Registers[LRegD];
      ocPush: begin
        Memory[Registers[LRegD]] := Registers[LRegA];
        if LRegD <> regZero then
          Registers[LRegD] += 1;
      end;
      ocPop: begin
        if LRegD <> regZero then
          Registers[LRegD] -= 1;
        Registers[LRegA] := Memory[Registers[LRegD]];
      end;
      ocCall: begin
        Memory[Registers[LRegD]] := Registers[regProgramCounter];
        if LRegD <> regZero then
          Registers[LRegD] += 1;
        Registers[regProgramCounter] := Registers[LRegA];
      end;
      ocReserved13:
        Halt := true;
    end;
  end;
end;

procedure AssembleProgram(var AContext: TExecutionContext);
var
  ProgramCounter: TLongTryte;

  procedure OpReg(AOpcode: TOpcode; ARegD: TRegister; ARegA: TRegister; ARegB: TRegister);
  begin
    AContext.Memory[ProgramCounter] :=
      (LongInt(AOpcode) * 19683) + { << 9 }
      (LongInt(ARegD)   *   729) + { << 6 }
      (LongInt(ARegA)   *    27) + { << 3 }
      (LongInt(ARegB)   *     1);  { << 0 }
    ProgramCounter += 1;
  end;

  procedure OpImm(AOpcode: TOpcode; ARegD: TRegister; AImmediate: THalfTryte);
  begin
    AContext.Memory[ProgramCounter] :=
      (LongInt(AOpcode)    * 19683) + { << 9 }
      (LongInt(ARegD)      *   729) + { << 6 }
      (LongInt(AImmediate) *     1);  { << 0 }
    ProgramCounter += 1;
  end;

begin
  ProgramCounter := 0;

  OpImm(ocLoadHighImmediate, regUser1, -364);
  OpImm(ocAddImmediate, regUser1, 364);

  OpReg(ocAddRegister, regUser12, regZero, regZero);
  OpReg(ocAddRegister, regUser13, regZero, regZero);
  OpImm(ocAddImmediate, regUser12, 1);
  OpImm(ocAddImmediate, regUser13, -2);

  OpReg(ocAddRegister, regUser2, regZero, regZero);
  OpReg(ocAddRegister, regUser3, regZero, regZero);

  OpReg(ocAddRegister, regUser2, regUser2, regUser12);
  OpReg(ocAddRegister, regUser3, regUser3, regUser13);

  OpImm(ocAddImmediate, regProgramCounter, -3);
  OpReg(ocReserved13, regZero, regZero, regZero);
end;

var
  Context: TExecutionContext;
begin
  {Write(#$1B'c');}
  InitContext(Context);
  AssembleProgram(Context);
  while not Context.Halt do begin
    {Write(#$1B'[1;1H');}
    PrintContext(Context);
    ExecuteContext(Context);
  end;
end.

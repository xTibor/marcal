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

  R0 - Zero
  R1 - Program counter
  R2 - Stack pointer

  Integer overflow -> crash
}

type
  TOpcode = (
    ocReserved13           = -13,
    ocReserved12           = -12,
    ocReserved11           = -11,
    ocReserved10           = -10,
    ocReserved9            =  -9,
    ocReserved8            =  -8,
    ocReserved7            =  -7,
    ocReserved6            =  -6,
    ocReserved5            =  -5,
    ocReserved4            =  -4,
    ocReserved3            =  -3,
    ocReserved2            =  -2,
    ocReserved1            =  -1,
    ocNop                  =   0,
    ocLoadImmediate        =   1,
    ocLoadHighImmediate    =   2,
    ocAddImmediate         =   3,
    ocAddRegister          =   4,
    ocLoadMemory           =   5,
    ocStoreMemory          =   6,
    ocBranchEquals         =   7,
    ocBranchNotEquals      =   8,
    ocBranchLessThan       =   9,
    ocBranchLessEqualsThan =  10
    { TODO }
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
    regStackPointer   =   2,
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
  LProgramCounter := AContext.Memory[AContext.Registers[regProgramCounter]];
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

  AContext.Registers[regProgramCounter] += 1;

  case LOpcode of
    ocLoadImmediate:
      if LRegD <> regZero then
        AContext.Registers[LRegD] := LImmediate;
    ocLoadHighImmediate:
      if LRegD <> regZero then
        AContext.Registers[LRegD] := LImmediate * 729; { << 6 }
    ocAddImmediate:
      if LRegD <> regZero then
        AContext.Registers[LRegD] += LImmediate;
    ocAddRegister:
      if LRegD <> regZero then
        AContext.Registers[LRegD] := AContext.Registers[LRegA] + AContext.Registers[LRegB];
    ocLoadMemory:
      if LRegD <> regZero then
        AContext.Registers[LRegD] := AContext.Memory[AContext.Registers[LRegA] + AContext.Registers[LRegB]];
    ocStoreMemory:
      AContext.Memory[AContext.Registers[LRegA] + AContext.Registers[LRegB]] := AContext.Registers[LRegD];
    ocBranchEquals:
      if AContext.Registers[LRegA] = AContext.Registers[LRegB] then
        AContext.Registers[regProgramCounter] := AContext.Registers[LRegD];
    ocBranchNotEquals:
      if AContext.Registers[LRegA] <> AContext.Registers[LRegB] then
        AContext.Registers[regProgramCounter] := AContext.Registers[LRegD];
    ocBranchLessThan:
      if AContext.Registers[LRegA] < AContext.Registers[LRegB] then
        AContext.Registers[regProgramCounter] := AContext.Registers[LRegD];
    ocBranchLessEqualsThan:
      if AContext.Registers[LRegA] <= AContext.Registers[LRegB] then
        AContext.Registers[regProgramCounter] := AContext.Registers[LRegD];
    ocReserved13:
      AContext.Halt := true;
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

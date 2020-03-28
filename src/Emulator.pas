{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}

program Emulator;

uses
  Trit, Words;

{
  When registers or immediate arguments not present, use implicit 0 arguments.
  Integer overflow -> crash

  Dyadic functions: DYAD RD RA RB
    In:
      RD: Register containing the truth table
      RA: Argument value register
      RB: Argument value register
    Out:
      RD: Output value register
      RD := TruthTable(RD)[RA, RB]
    Truth table representation:
            RB RB RB
            [-][0][+]
      RA [-] a  b  c
      RA [0] d  e  f
      RA [+] g  h  i
      where RD := [000ihgfedcba]

  Shift/Rotate:
    Operates in a +12 .. -12 range
    Outside it has an undefined behaviour
}

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

  TExecutionContext = record
    Registers: array[TRegister] of TWord;
    Memory: array[TWord] of TWord;
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
  LValue: TWord;
begin
  LValue := AContext.Registers[regZero];
  WriteLn(WordToStr(LValue), ' (', LValue:7, ') ');

  for LIndex := 1 to 13 do begin
    LValue := AContext.Registers[TRegister(+LIndex)];
    Write(WordToStr(LValue), ' (', LValue:7, ') ');

    LValue := AContext.Registers[TRegister(-LIndex)];
    Write(WordToStr(LValue), ' (', LValue:7, ') ');

    WriteLn();
  end;
  WriteLn();
end;

procedure ExecuteContext(var AContext: TExecutionContext);
var
  LProgramCounter: TWord;

  LQuarterWords: TQuarterWordArray;
  LOpcode: TOpcode;
  LRegA: TRegister;
  LRegB: TRegister;
  LRegD: TRegister;

  LHalfWords: THalfWordArray;
  LImmediateQuarter: TQuarterWord;
  LImmediateHalf: THalfWord;
begin
  with AContext do begin
    LProgramCounter := Memory[Registers[regProgramCounter]];
    LQuarterWords := WordToQuarterWords(LProgramCounter);
    LHalfWords  := WordToHalfWords(LProgramCounter);

    LOpcode    := TOpcode(LQuarterWords[3]);
    LRegD      := TRegister(LQuarterWords[2]);
    LRegA      := TRegister(LQuarterWords[1]);
    LRegB      := TRegister(LQuarterWords[0]);

    LImmediateQuarter := LQuarterWords[0];
    LImmediateHalf := LHalfWords[0];

    WriteLn(LOpcode, ' ', LRegD, ' ', LRegA, ' ', LRegB);
    WriteLn(WordToStr(LImmediateHalf), ' (', LImmediateHalf:7, ') ');
    WriteLn();

    Registers[regProgramCounter] += 1;

    case LOpcode of
      ocNegation:
        if LRegD <> regZero then
          Registers[LRegD] := WordApplyMonadicFunction(Registers[LRegA], CTritFunctionNegation);
      ocLoadLowImmediate:
        if LRegD <> regZero then
          Registers[LRegD] := LImmediateHalf;
      ocLoadHighImmediate:
        if LRegD <> regZero then
          Registers[LRegD] := LImmediateHalf * 729; { << 6 }
      ocAddImmediateQuarter:
        if LRegD <> regZero then
          Registers[LRegD] := Registers[LRegA] + LImmediateQuarter;
      ocAddImmediateHalf:
        if LRegD <> regZero then
          Registers[LRegD] += LImmediateHalf;
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
      ocPull: begin
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
      ocDyadicFunction: begin
        if LRegD <> regZero then
          Registers[LRegD] := WordApplyDyadicFunction(
            Registers[LRegA],
            Registers[LRegB],
            WordToDyadicFunction(Registers[LRegD]));
      end;
      ocShift:
        if LRegD <> regZero then
          Registers[LRegD] := WordShift(Registers[LRegA], Registers[LRegB]);
      ocRotate:
        if LRegD <> regZero then
          Registers[LRegD] := WordRotate(Registers[LRegA], Registers[LRegB]);
      ocReserved13:
        Halt := true;
    end;
  end;
end;

procedure AssembleProgram(var AContext: TExecutionContext);
var
  ProgramCounter: TWord;

  procedure OpRgtr(AOpcode: TOpcode; ARegD: TRegister; ARegA: TRegister; ARegB: TRegister);
  begin
    AContext.Memory[ProgramCounter] :=
      (LongInt(AOpcode) * 19683) + { << 9 }
      (LongInt(ARegD)   *   729) + { << 6 }
      (LongInt(ARegA)   *    27) + { << 3 }
      (LongInt(ARegB)   *     1);  { << 0 }
    ProgramCounter += 1;
  end;

  procedure OpImm3(AOpcode: TOpcode; ARegD: TRegister; ARegA: TRegister; AImmediate: TQuarterWord);
  begin
    AContext.Memory[ProgramCounter] :=
      (LongInt(AOpcode)    * 19683) + { << 9 }
      (LongInt(ARegD)      *   729) + { << 6 }
      (LongInt(ARegA)      *    27) + { << 3 }
      (LongInt(AImmediate) *     1);  { << 0 }
    ProgramCounter += 1;
  end;

  procedure OpImm6(AOpcode: TOpcode; ARegD: TRegister; AImmediate: THalfWord);
  begin
    AContext.Memory[ProgramCounter] :=
      (LongInt(AOpcode)    * 19683) + { << 9 }
      (LongInt(ARegD)      *   729) + { << 6 }
      (LongInt(AImmediate) *     1);  { << 0 }
    ProgramCounter += 1;
  end;

begin
  ProgramCounter := 0;

  (*
  OpImm6(ocLoadHighImmediate,    regUser11, 14);                   { LDW U11 10000     } {         }
  OpImm6(ocAddImmediateHalf,     regUser11, -206);                                       {         }
  OpImm6(ocLoadLowImmediate,     regUser1, 0);                     { LDLH U1 0         } {         }
  OpImm6(ocLoadLowImmediate,     regUser2, 0);                     { LDLH U2 0         } {         }
  OpImm6(ocAddImmediateHalf,     regUser1, 1);                     { ADDH U1 1         } { <<<<<<+ }
  OpImm3(ocAddImmediateQuarter,  regUser12, regProgramCounter, 4); { ADDQ U12 S1 4     } {       ^ }
  OpRgtr(ocBranchNotEquals,      regUser12, regUser11, regUser1);  { BRNE U12 U11 U1   } { >>+   ^ }
  OpImm6(ocLoadHighImmediate,    regUser1, -14);                   { LDW U1 -10000     } {   V   ^ }
  OpImm6(ocAddImmediateHalf,     regUser1, 206);                                         {   V   ^ }
  OpImm6(ocAddImmediateHalf,     regUser2, 1);                     { ADDH U2 1         } {   V   ^ }
  OpImm6(ocAddImmediateHalf,     regProgramCounter, -7);           { ADDH S1 -7        } { <<+ >>+ }
  // *)

  //(*
  OpImm6(ocLoadHighImmediate,    regUser1, 13);                    { LDW U1 9464       } {         } { LOAD OPERAND A: 000+++000---             }
  OpImm6(ocAddImmediateHalf,     regUser1, -13);                                         {         }
  OpImm6(ocLoadHighImmediate,    regUser2, 224);                   { LDW U2 163520     } {         } { LOAD OPERAND B: +0-+0-+0-+0-             }
  OpImm6(ocAddImmediateHalf,     regUser2, 224);                                         {         }
  OpImm6(ocLoadHighImmediate,    regUser10, 5);                    { LDW U10 3445      } {         } { LOAD TRUTH TABLE FOR TRITWISE EQUALITY   }
  OpImm6(ocAddImmediateHalf,     regUser10, -200);                                       {         }
  OpRgtr(ocAddRegister,          regUser3, regUser10, regZero);    { MOVR U3 U10       } {         } { MOVR TRUTH TABLE TO DESTINATION REGISTER }
  OpRgtr(ocDyadicFunction,       regUser3, regUser1, regUser2);    { DYAD U3 U1 U2     } {         } { PERFORM TRITWISE EQUALITY OPERATION      }
  OpRgtr(ocReserved13,           regZero, regZero, regZero);       { HALT              } {         }
  // *)

  (*
  OpImm6(ocLoadHighImmediate,    regUser1, 224);                   { LDW U2 163520     } {         } { LOAD TEST WORD WITH NICE PATTERN         }
  OpImm6(ocAddImmediateHalf,     regUser1, 224);                                         {         }
  OpImm6(ocLoadLowImmediate,     regUser2, -12);                   { LDLH U2 -12       } {         } { CURRENT SHIFT AMOUNT                     }
  OpImm6(ocLoadLowImmediate,     regUser3, 12);                    { LDLH U3 12        } {         } { MAX SHIFT AMOUNT                         }
  OpImm3(ocAddImmediateQuarter,  regUser4, regProgramCounter, 0);  { MOVR U4 S1        } {         } { SET BRANCH TARGET TO NEXT INSTRUCTION    }
  OpRgtr(ocShift,                regUser10, regUser1, regUser2);   { LSHR U10 U1 U2    } { <<+     }
  OpRgtr(ocRotate,               regUser11, regUser1, regUser2);   { ROTR U11 U1 U2    } {   ^     }
  OpImm3(ocAddImmediateQuarter,  regUser2, regUser2, 1);           { ADDQ U2 U2 1      } {   ^     }
  OpRgtr(ocBranchLessEqualsThan, regUser4, regUser2, regUser3);    { BRNE U4 U2 U3     } { >>+     }
  OpRgtr(ocReserved13,           regZero, regZero, regZero);       { HALT              } {         }
  // *)
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

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
  Instruction formats:
    RGTR:
      000 000 000 +--
      OP  RD  RA  RB
    IMM3:
      000 000 000 000
      OP  RD  RA  IMM
    IMM6:
      000 000 000 000
      OP  RD  IMM-IMM

  When registers or immediate arguments not present, use implicit 0 arguments.

  R0 - Zero
  R1 - Program counter

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
    ocShift                =  -3, { RGTR - SHFT RD RA RB }
    ocNegation             =  -2, { RGTR - NEGR RD RA    }
    ocDyadicFunction       =  -1, { RGTR - DYAD RD RA RB }
    ocAddRegister          =   0, { RGTR - ADDR RD RA RB }
    ocAddImmediateShort    =   1, { IMM3 - ADSI RD RA 12 }
    ocAddImmediateHalf     =   2, { IMM6 - ADHI RD 123   }
    ocLoadLowImmediate     =   3, { IMM6 - LDLI RD 123   }
    ocLoadHighImmediate    =   4, { IMM6 - LDHI RD 123   }
    ocLoadMemory           =   5, { RGTR - LDMR RD RA RB }
    ocStoreMemory          =   6, { RGTR - STMR RD RA RB }
    ocBranchEquals         =   7, { RGTR - BREQ RD RA RB }
    ocBranchNotEquals      =   8, { RGTR - BRNE RD RA RB }
    ocBranchLessThan       =   9, { RGTR - BRLT RD RA RB }
    ocBranchLessEqualsThan =  10, { RGTR - BRLE RD RA RB }
    ocPush                 =  11, { RGTR - PSHR SP RA    }
    ocPop                  =  12, { RGTR - POPR SP RA    }
    ocCall                 =  13  { RGTR - CALL SP RA    }
    {
      General pseudo opcodes - - - - - - - - - - - - - - - - - - - - - - - - - -

        NoOperation              NOOP            => ADDR R0 R0 R0

        Move                     MOVR RD RA      => ADDR RD RA R0

        Return                   RTRN SP         => POPR SP PC

        BranchGreaterThan        BRGT RD RA RB   => BRLE RD RB RA

        BranchGreaterEqualsThan  BRGE RD RA RB   => BRLT RD RB RA

        LoadImmediate            LDI RD -264992  => LDHI RD -364
                                                    ADHI RD 364

        LoadMemory               LDM RD 212686   => LDHI RD 292
                                                    ADHI RD -182
                                                    LDMR RD RD R0

        Subtraction              SUBR RD RA RB   => NEGR RD RB
                                                    ADDR RD RD RA

        StackRelativeLoad        STCK SP RD -1   => ADSI RD SP -1
                                                    LDMR RD RD R0

      Two-operand functions using DYAD - - - - - - - - - - - - - - - - - - - - -

        LogicalAnd               AND RD RA RB    => LDHI RD 8
                                                    ADHI RD -40
                                                    DYAD RD RA RB

        LogicalNand              NAND RD RA RB   => LDHI RD -8
                                                    ADHI RD 40
                                                    DYAD RD RA RB

        LogicalOr                OR RD RA RB     => LDHI RD 13
                                                    ADHI RD 251
                                                    DYAD RD RA RB

        LogicalNor               NOR RD RA RB    => LDHI RD -13
                                                    ADHI RD -251
                                                    DYAD RD RA RB

        LogicalXor               XOR RD RA RB    => LDHI RD -8
                                                    ADHI RD 8
                                                    DYAD RD RA RB

        LogicalXnor              XNOR RD RA RB   => LDHI RD 8
                                                    ADHI RD -8
                                                    DYAD RD RA RB

        Equality                 TEQ RD RA RB    => LDHI RD 5
                                                    ADHI RD -200
                                                    DYAD RD RA RB

        LessThan                 TLT RD RA RB    => LDHI RD -13
                                                    ADHI RD 146
                                                    DYAD RD RA RB

        LessEqualsThan           TLE RD RA RB    => LDHI RD 5
                                                    ADHI RD 310
                                                    DYAD RD RA RB

        GreaterThan              TGT RD RA RB    => LDHI RD -5
                                                    ADHI RD -310
                                                    DYAD RD RA RB

        GreaterEqualsThan        TGE RD RA RB    => LDHI RD 13
                                                    ADHI RD -146
                                                    DYAD RD RA RB

        Consensus                CSS RD RA RB    => LDHI RD 9
                                                    ADHI RD -1
                                                    DYAD RD RA RB

        AcceptAnything           ANY RD RA RB    => LDHI RD 12
                                                    ADHI RD 212
                                                    DYAD RD RA RB

        KleeneImplication        KIMP RD RA RB   => LDHI RD 8
                                                    ADHI RD 256
                                                    DYAD RD RA RB

        LukasiewiczImplication   LIMP RD RA RB   => LDHI RD 8
                                                    ADHI RD 337
                                                    DYAD RD RA RB

      One-operand functions using DYAD - - - - - - - - - - - - - - - - - - - - -

        NegativeThresholdInvert  NTI RD RA       => LDHI RD -3
                                                    ADHI RD -78
                                                    DYAD RD RA R0

        PositiveThresholdInvert  PTI RD RA       => LDHI RD -3
                                                    ADHI RD 84
                                                    DYAD RD RA R0

        TritwiseIncrement        TWI RD RA       => LDHI RD -3
                                                    ADHI RD 81
                                                    DYAD RD RA R0

        TritwiseDecrement        TWD RD RA       => LDHI RD 0
                                                    ADHI RD -78
                                                    DYAD RD RA R0

        TritwiseIsFalse          TIF RD RA       => LDHI RD -3
                                                    ADHI RD -78
                                                    DYAD RD RA R0

        TritwiseIsUnknown        TIU RD RA       => LDHI RD -3
                                                    ADHI RD 78
                                                    DYAD RD RA R0

        TritwiseIsTrue           TIT RD RA       => LDHI RD 3
                                                    ADHI RD -84
                                                    DYAD RD RA R0

        TritwiseClampDown        TCD RD RA       => LDHI RD 0
                                                    ADHI RD -3
                                                    DYAD RD RA R0

        TritwiseClampUp          TCU RD RA       => LDHI RD 3
                                                    ADHI RD 0
                                                    DYAD RD RA R0
    }
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
  LImmediateShort: TShortTryte;
  LImmediateHalf: THalfTryte;
begin
  with AContext do begin
    LProgramCounter := Memory[Registers[regProgramCounter]];
    LShortSplice := LongTryteShortSplice(LProgramCounter);
    LHalfSplice  := LongTryteHalfSplice(LProgramCounter);

    LOpcode    := TOpcode(LShortSplice[3]);
    LRegD      := TRegister(LShortSplice[2]);
    LRegA      := TRegister(LShortSplice[1]);
    LRegB      := TRegister(LShortSplice[0]);

    LImmediateShort := LShortSplice[0];
    LImmediateHalf := LHalfSplice[0];

    WriteLn(LOpcode, ' ', LRegD, ' ', LRegA, ' ', LRegB);
    WriteLn(LongTryteToStr(LImmediateHalf), ' (', LImmediateHalf:7, ') ');
    WriteLn();

    Registers[regProgramCounter] += 1;

    case LOpcode of
      ocNegation:
        if LRegD <> regZero then
          Registers[LRegD] := LongTryteApplyMonadicFunction(Registers[LRegA], CTritFunctionNegation);
      ocLoadLowImmediate:
        if LRegD <> regZero then
          Registers[LRegD] := LImmediateHalf;
      ocLoadHighImmediate:
        if LRegD <> regZero then
          Registers[LRegD] := LImmediateHalf * 729; { << 6 }
      ocAddImmediateShort:
        if LRegD <> regZero then
          Registers[LRegD] := Registers[LRegA] + LImmediateShort;
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
      ocDyadicFunction: begin
        if LRegD <> regZero then
          Registers[LRegD] := LongTryteApplyDyadicFunction(
            Registers[LRegA],
            Registers[LRegB],
            LongTryteToDyadicFunction(Registers[LRegD]));
      end;
      ocShift:
        if LRegD <> regZero then
          Registers[LRegD] := LongTryteShift(Registers[LRegA], Registers[LRegB]);
      ocRotate:
        if LRegD <> regZero then
          Registers[LRegD] := LongTryteRotate(Registers[LRegA], Registers[LRegB]);
      ocReserved13:
        Halt := true;
    end;
  end;
end;

procedure AssembleProgram(var AContext: TExecutionContext);
var
  ProgramCounter: TLongTryte;

  procedure OpRgtr(AOpcode: TOpcode; ARegD: TRegister; ARegA: TRegister; ARegB: TRegister);
  begin
    AContext.Memory[ProgramCounter] :=
      (LongInt(AOpcode) * 19683) + { << 9 }
      (LongInt(ARegD)   *   729) + { << 6 }
      (LongInt(ARegA)   *    27) + { << 3 }
      (LongInt(ARegB)   *     1);  { << 0 }
    ProgramCounter += 1;
  end;

  procedure OpImm3(AOpcode: TOpcode; ARegD: TRegister; ARegA: TRegister; AImmediate: TShortTryte);
  begin
    AContext.Memory[ProgramCounter] :=
      (LongInt(AOpcode)    * 19683) + { << 9 }
      (LongInt(ARegD)      *   729) + { << 6 }
      (LongInt(ARegA)      *    27) + { << 3 }
      (LongInt(AImmediate) *     1);  { << 0 }
    ProgramCounter += 1;
  end;

  procedure OpImm6(AOpcode: TOpcode; ARegD: TRegister; AImmediate: THalfTryte);
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
  OpImm6(ocLoadHighImmediate,    regUser11, 14);                   { LDI U11 10000     } {         }
  OpImm6(ocAddImmediateHalf,     regUser11, -206);                                       {         }
  OpImm6(ocLoadLowImmediate,     regUser1, 0);                     { LDLI U1 0         } {         }
  OpImm6(ocLoadLowImmediate,     regUser2, 0);                     { LDLI U2 0         } {         }
  OpImm6(ocAddImmediateHalf,     regUser1, 1);                     { ADHI U1 1         } { <<<<<<+ }
  OpImm3(ocAddImmediateShort,    regUser12, regProgramCounter, 4); { ADSI U12 S1 4     } {       ^ }
  OpRgtr(ocBranchNotEquals,      regUser12, regUser11, regUser1);  { BRNE U12 U11 U1   } { >>+   ^ }
  OpImm6(ocLoadHighImmediate,    regUser1, -14);                   { LDI U1 -10000     } {   V   ^ }
  OpImm6(ocAddImmediateHalf,     regUser1, 206);                                         {   V   ^ }
  OpImm6(ocAddImmediateHalf,     regUser2, 1);                     { ADHI U2 1         } {   V   ^ }
  OpImm6(ocAddImmediateHalf,     regProgramCounter, -7);           { ADHI S1 -7        } { <<+ >>+ }
  // *)

  //(*
  OpImm6(ocLoadHighImmediate,    regUser1, 13);                    { LDI U1 9464       } {         } { LOAD OPERAND A: 000+++000---             }
  OpImm6(ocAddImmediateHalf,     regUser1, -13);                                         {         }
  OpImm6(ocLoadHighImmediate,    regUser2, 224);                   { LDI U2 163520     } {         } { LOAD OPERAND B: +0-+0-+0-+0-             }
  OpImm6(ocAddImmediateHalf,     regUser2, 224);                                         {         }
  OpImm6(ocLoadHighImmediate,    regUser10, 5);                    { LDI U10 3445      } {         } { LOAD TRUTH TABLE FOR TRITWISE EQUALITY   }
  OpImm6(ocAddImmediateHalf,     regUser10, -200);                                       {         }
  OpRgtr(ocAddRegister,          regUser3, regUser10, regZero);    { MOVR U3 U10       } {         } { MOVE TRUTH TABLE TO DESTINATION REGISTER }
  OpRgtr(ocDyadicFunction,       regUser3, regUser1, regUser2);    { DYAD U3 U1 U2     } {         } { PERFORM TRITWISE EQUALITY OPERATION      }
  OpRgtr(ocReserved13,           regZero, regZero, regZero);       { HALT              } {         }
  // *)

  (*
  OpImm6(ocLoadHighImmediate,    regUser1, 224);                   { LDI U2 163520     } {         } { LOAD TEST TRYTE WITH NICE PATTERN        }
  OpImm6(ocAddImmediateHalf,     regUser1, 224);                                         {         }
  OpImm6(ocLoadLowImmediate,     regUser2, -12);                   { LDLI U2 -12       } {         } { CURRENT SHIFT AMOUNT                     }
  OpImm6(ocLoadLowImmediate,     regUser3, 12);                    { LDLI U3 12        } {         } { MAX SHIFT AMOUNT                         }
  OpImm3(ocAddImmediateShort,    regUser4, regProgramCounter, 0);  { MOVR U4 S1        } {         } { SET BRANCH TARGET TO NEXT INSTRUCTION    }
  OpRgtr(ocShift,                regUser10, regUser1, regUser2);   { SHFT U10 U1 U2    } { <<+     }
  OpRgtr(ocRotate,               regUser11, regUser1, regUser2);   { ROTR U11 U1 U2    } {   ^     }
  OpImm3(ocAddImmediateShort,    regUser2, regUser2, 1);           { ADSI U2 U2 1      } {   ^     }
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

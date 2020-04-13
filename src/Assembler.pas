{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}

program Assembler;

uses
  Trit, Words, Arch, Utils, SysUtils;

function EncodeInstructionRgtr(AOpcode: TInstructionOpcode; ARegD: TRegister; ARegA: TRegister; ARegB: TRegister): TWord;
begin
  EncodeInstructionRgtr :=
    (LongInt(AOpcode) * 19683) + { << 9 }
    (LongInt(ARegD)   *   729) + { << 6 }
    (LongInt(ARegA)   *    27) + { << 3 }
    (LongInt(ARegB)   *     1);  { << 0 }
end;

function EncodeInstructionImm3(AOpcode: TInstructionOpcode; ARegD: TRegister; ARegA: TRegister; AImmediate: TQuarterWord): TWord;
begin
  EncodeInstructionImm3 :=
    (LongInt(AOpcode)    * 19683) + { << 9 }
    (LongInt(ARegD)      *   729) + { << 6 }
    (LongInt(ARegA)      *    27) + { << 3 }
    (LongInt(AImmediate) *     1);  { << 0 }
end;

function EncodeInstructionImm6(AOpcode: TInstructionOpcode; ARegD: TRegister; AImmediate: THalfWord): TWord;
begin
  EncodeInstructionImm6 :=
    (LongInt(AOpcode)    * 19683) + { << 9 }
    (LongInt(ARegD)      *   729) + { << 6 }
    (LongInt(AImmediate) *     1);  { << 0 }
end;

var
  GInputFile: TextFile;
  GOutputFile: TextFile;
  GLine: String;
  GLineSplit: TStringArray;
  GOpcode: TInstructionOpcode;
  GProgramCounter: TWord;
  GInstruction: TWord;

begin
  if ParamCount() <> 2 then begin
    WriteLn('Usage: Assembler input.s output.t');
    Exit;
  end;

  Assign(GInputFile, ParamStr(1));
  Assign(GOutputFile, ParamStr(2));
  Reset(GInputFile);
  Rewrite(GOutputFile);

  GProgramCounter := 0;

  while not Eof(GInputFile) do begin
    ReadLn(GInputFile, GLine);
    GLineSplit := Split(GLine);

    GOpcode := StrToInstructionOpcode(GLineSplit[0]);
    case CInstructionFormats[GOpcode] of
      ifRegister:
        GInstruction := EncodeInstructionRgtr(
          GOpcode,
          StrToRegister(GLineSplit[1]),
          StrToRegister(GLineSplit[2]),
          StrToRegister(GLineSplit[3]));
      ifImmediate3:
        GInstruction := EncodeInstructionImm3(
          GOpcode,
          StrToRegister(GLineSplit[1]),
          StrToRegister(GLineSplit[2]),
          StrToInt(GLineSplit[3]));
      ifImmediate6:
        GInstruction := EncodeInstructionImm6(
          GOpcode,
          StrToRegister(GLineSplit[1]),
          StrToInt(GLineSplit[2]));
    end;

    WriteLn(GOutputFile, GProgramCounter, ' ', GInstruction);
    GProgramCounter += 1;
  end;

  Close(GInputFile);
  Close(GOutputFile);
end.

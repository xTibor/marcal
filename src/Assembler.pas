{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}

program Assembler;

uses
  Trit, Words, Arch, Utils, SysUtils, Fgl, StrUtils;

type
  TSymbolTable = specialize TFPGMap<String, TWord>;

const
  CLabelWidth       = 11;
  CInstructionWidth = 35;
  CCommentWidth     = 35;
  CLineWidth = CLabelWidth + CInstructionWidth + CCommentWidth;

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
  GSymbolTable: TSymbolTable;
  GLine: String;
  GStrLabel: String;
  GStrInstruction: String;
  GStrComment: String;
  GParts: TStringArray;
  GOpcode: TInstructionOpcode;
  GProgramCounter: TWord;
  GInstruction: TWord;
  GIndex: Integer;

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
  GSymbolTable := TSymbolTable.Create();

  while not Eof(GInputFile) do begin
    ReadLn(GInputFile, GLine);

    GLine := PadRight(GLine, CLineWidth);
    GLine := Copy(GLine, 1, CLineWidth);

    GStrLabel       := Trim(ChompLeft(GLine, CLabelWidth      ));
    GStrInstruction := Trim(ChompLeft(GLine, CInstructionWidth));
    GStrComment     := Trim(ChompLeft(GLine, CCommentWidth    ));

    if GStrLabel <> '' then begin
      GSymbolTable.Add(GStrLabel, GProgramCounter);
    end;

    if GStrInstruction <> '' then begin
      GParts := Split(GStrInstruction);

      GOpcode := StrToInstructionOpcode(GParts[0]);
      case CInstructionFormats[GOpcode] of
        ifRegister:
          GInstruction := EncodeInstructionRgtr(
            GOpcode,
            StrToRegister(GParts[1]),
            StrToRegister(GParts[2]),
            StrToRegister(GParts[3]));
        ifImmediate3:
          GInstruction := EncodeInstructionImm3(
            GOpcode,
            StrToRegister(GParts[1]),
            StrToRegister(GParts[2]),
            StrToInt(GParts[3]));
        ifImmediate6:
          GInstruction := EncodeInstructionImm6(
            GOpcode,
            StrToRegister(GParts[1]),
            StrToInt(GParts[2]));
      end;

      WriteLn(GOutputFile, GProgramCounter, ' ', GInstruction);
      GProgramCounter += 1;
    end;
  end;

  Close(GInputFile);
  Close(GOutputFile);

  for GIndex := 0 to GSymbolTable.Count - 1 do begin
    WriteLn(Format('%:-*s%d', [CLabelWidth, GSymbolTable.Keys[GIndex], GSymbolTable.Data[GIndex]]));
  end;
end.

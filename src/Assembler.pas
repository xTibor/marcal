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

type
  TAssemblerFunction = (
    afNone,
    afPcRelative1,
    afPcRelative2,
    afLowHalf,
    afHighHalf
  );

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

procedure SplitLine(ALine: String; var ALabel: String; var AInstruction: String);
begin
  ALine := PadRight(ALine, CLineWidth);
  ALine := Copy(ALine, 1, CLineWidth);

  ALabel       := Trim(ChompLeft(ALine, CLabelWidth      ));
  AInstruction := Trim(ChompLeft(ALine, CInstructionWidth));
end;

var
  GOutputFile: TextFile;

  GLines: TStringArray;
  GLineIndex: Integer;

  GProgramCounter: TWord;
  GSymbolTable: TSymbolTable;

  GStrLabel: String;
  GStrInstruction: String;

  GFunction: TAssemblerFunction;
  GHalfWords: THalfWordArray;
  GStrParts: TStringArray;
  GWordParts: array of TWord;
  GInteger: LongInt;
  GInstruction: TWord;

  GIndex: Integer;
  GRegisterIndex: TRegister;
  GOpcodeIndex: TInstructionOpcode;
begin
  if ParamCount() <> 2 then begin
    WriteLn('Usage: Assembler input.s output.t');
    Exit;
  end;

  GLines := ReadFileByLines(ParamStr(1));
  GSymbolTable := TSymbolTable.Create();

  { Prefill symbol table }

  for GOpcodeIndex := Low(TInstructionOpcode) to High(TInstructionOpcode) do
    GSymbolTable.Add(CInstructionMnemonics[GOpcodeIndex], TWord(GOpcodeIndex));

  for GRegisterIndex := Low(TRegister) to High(TRegister) do begin
    for GIndex := Low(CRegisterNames[GRegisterIndex]) to High(CRegisterNames[GRegisterIndex]) do begin
      if CRegisterNames[GRegisterIndex][GIndex] <> '' then
        GSymbolTable.Add(CRegisterNames[GRegisterIndex][GIndex], TWord(GRegisterIndex));
    end;
  end;

  { First pass: Find all symbols }

  GProgramCounter := 0;
  for GLineIndex := Low(GLines) to High(GLines) do begin
    SplitLine(GLines[GLineIndex], GStrLabel, GStrInstruction);

    if GStrLabel <> '' then begin
      if GSymbolTable.IndexOf(GStrLabel) <> -1 then begin
        WriteLn(Format('Symbol redefinition at line %d: %s', [GLineIndex + 1, GStrLabel]));
        Halt(1);
      end;

      GSymbolTable.Add(GStrLabel, GProgramCounter);
    end;

    if GStrInstruction <> '' then
      GProgramCounter += 1;
  end;

  { Second pass: Assemble instructions }

  Assign(GOutputFile, ParamStr(2));
  Rewrite(GOutputFile);

  GProgramCounter := 0;
  for GLineIndex := Low(GLines) to High(GLines) do begin
    SplitLine(GLines[GLineIndex], GStrLabel, GStrInstruction);

    if GStrInstruction <> '' then begin
      GStrParts := Split(GStrInstruction);

      { Limit 4 parts per instruction }
      if Length(GStrParts) > 4 then
        SetLength(GStrParts, 4);

      SetLength(GWordParts, Length(GStrParts));
      for GIndex := Low(GStrParts) to High(GStrParts) do begin
        { Read function markers }
        case GStrParts[GIndex][1] of
          '''': GFunction := afPcRelative1;
          '"':  GFunction := afPcRelative2;
          '>':  GFunction := afHighHalf;
          '<':  GFunction := afLowHalf;
          else  GFunction := afNone;
        end;

        if GFunction <> afNone then
          Delete(GStrParts[GIndex], 1, 1);

        if TryStrToInt(GStrParts[GIndex], GInteger) then begin
          { Is it a number? }
          GWordParts[GIndex] := TWord(GInteger);
        end else if GSymbolTable.IndexOf(GStrParts[GIndex]) <> -1 then begin
          { Is it a known symbol? }
          GWordParts[GIndex] := GSymbolTable[GStrParts[GIndex]];
        end else begin
          { Unknown }
          WriteLn(Format('Syntax error at line %d: %s', [GLineIndex + 1, GStrParts[GIndex]]));
          Halt(1);
        end;

        { Apply function }
        case GFunction of
          afPcRelative1: begin
            GWordParts[GIndex] := GWordParts[GIndex] - (GProgramCounter + 1);
          end;
          afPcRelative2: begin
            GWordParts[GIndex] := GWordParts[GIndex] - (GProgramCounter + 2);
          end;
          afHighHalf: begin
            GHalfWords := WordToHalfWords(GWordParts[GIndex]);
            GWordParts[GIndex] := GHalfWords[1];
          end;
          afLowHalf: begin
            GHalfWords := WordToHalfWords(GWordParts[GIndex]);
            GWordParts[GIndex] := GHalfWords[0];
          end;
        end;
      end;

      { Implicit zero arguments }
      SetLength(GWordParts, 4);

      { Emit the instruction }
      case CInstructionFormats[TInstructionOpcode(GWordParts[0])] of
        ifRegister:
          GInstruction := EncodeInstructionRgtr(
            TInstructionOpcode(GWordParts[0]),
            TRegister(GWordParts[1]),
            TRegister(GWordParts[2]),
            TRegister(GWordParts[3]));
        ifImmediate3:
          GInstruction := EncodeInstructionImm3(
            TInstructionOpcode(GWordParts[0]),
            TRegister(GWordParts[1]),
            TRegister(GWordParts[2]),
            TQuarterWord(GWordParts[3]));
        ifImmediate6:
          GInstruction := EncodeInstructionImm6(
            TInstructionOpcode(GWordParts[0]),
            TRegister(GWordParts[1]),
            THalfWord(GWordParts[2]));
      end;

      WriteLn(GOutputFile, GProgramCounter, ' ', GInstruction);
      GProgramCounter += 1;
    end;
  end;

  Close(GOutputFile);

  for GIndex := 0 to GSymbolTable.Count - 1 do begin
    WriteLn(Format('%:-*s%d', [CLabelWidth, GSymbolTable.Keys[GIndex], GSymbolTable.Data[GIndex]]));
  end;

end.

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

procedure SplitLine(ALine: String; var ALabel: String; var AInstruction: String);
begin
  ALine := PadRight(ALine, CLineWidth);
  ALine := Copy(ALine, 1, CLineWidth);

  ALabel       := Trim(ChompLeft(ALine, CLabelWidth      ));
  AInstruction := Trim(ChompLeft(ALine, CInstructionWidth));
end;

var
  GSymbolTable: TSymbolTable;
  GProgramCounter: TWord;
  GLineIndex: Integer;

function ParseValue(AString: String): TWord;
var
  LFunction: TAssemblerFunction;
  LInteger: LongInt;
  LHalfWords: THalfWordArray;
begin
  { Read function markers }
  case AString[1] of
    '''': LFunction := afPcRelative1;
    '"':  LFunction := afPcRelative2;
    '>':  LFunction := afHighHalf;
    '<':  LFunction := afLowHalf;
    else  LFunction := afNone;
  end;

  if LFunction <> afNone then
    Delete(AString, 1, 1);

  if TryStrToInt(AString, LInteger) then begin
    { Is it a number? }
    ParseValue := TWord(LInteger);
  end else if GSymbolTable.IndexOf(AString) <> -1 then begin
    { Is it a known symbol? }
    ParseValue := GSymbolTable[AString];
  end else begin
    { Unknown }
    WriteLn(Format('Syntax error at line %d: %s', [GLineIndex + 1, AString]));
    Halt(1);
  end;

  { Apply function }
  case LFunction of
    afPcRelative1: begin
      ParseValue := ParseValue - (GProgramCounter + 1);
    end;
    afPcRelative2: begin
      ParseValue := ParseValue - (GProgramCounter + 2);
    end;
    afHighHalf: begin
      LHalfWords := WordToHalfWords(ParseValue);
      ParseValue := LHalfWords[1];
    end;
    afLowHalf: begin
      LHalfWords := WordToHalfWords(ParseValue);
      ParseValue := LHalfWords[0];
    end;
  end;
end;

var
  GOutputFile: TextFile;
  GLines: TStringArray;
  GStrLabel: String;
  GStrInstruction: String;
  GStrParts: TStringArray;
  GWordParts: array of TWord;
  GOutputValue: TWord;
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

      if GStrParts[0] = 'DATA' then begin
        { Data definition }
        GOutputValue := ParseValue(GStrParts[1]);
      end else begin
        { Regular instructions }
        SetLength(GWordParts, Length(GStrParts));

        for GIndex := Low(GStrParts) to High(GStrParts) do
          GWordParts[GIndex] := ParseValue(GStrParts[GIndex]);

        { Implicit zero arguments }
        SetLength(GWordParts, 4);

        { Emit the instruction }
        case CInstructionFormats[TInstructionOpcode(GWordParts[0])] of
          ifRegister:
            GOutputValue := 
              (LongInt(TInstructionOpcode(GWordParts[0])) * 19683) + { << 9 }
              (LongInt(         TRegister(GWordParts[1])) *   729) + { << 6 }
              (LongInt(         TRegister(GWordParts[2])) *    27) + { << 3 }
              (LongInt(         TRegister(GWordParts[3])) *     1);  { << 0 }
          ifImmediate3:
            GOutputValue := 
              (LongInt(TInstructionOpcode(GWordParts[0])) * 19683) + { << 9 }
              (LongInt(         TRegister(GWordParts[1])) *   729) + { << 6 }
              (LongInt(         TRegister(GWordParts[2])) *    27) + { << 3 }
              (LongInt(      TQuarterWord(GWordParts[3])) *     1);  { << 0 }
          ifImmediate6:
            GOutputValue := 
              (LongInt(TInstructionOpcode(GWordParts[0])) * 19683) + { << 9 }
              (LongInt(         TRegister(GWordParts[1])) *   729) + { << 6 }
              (LongInt(         THalfWord(GWordParts[2])) *     1);  { << 0 }
        end;
      end;

      WriteLn(GOutputFile, GProgramCounter, ' ', GOutputValue);
      GProgramCounter += 1;
    end;
  end;

  Close(GOutputFile);

  for GIndex := 0 to GSymbolTable.Count - 1 do begin
    WriteLn(Format('%:-*s%d', [CLabelWidth, GSymbolTable.Keys[GIndex], GSymbolTable.Data[GIndex]]));
  end;

end.

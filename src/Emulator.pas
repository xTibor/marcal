{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}

program Emulator;

uses
  Trit, Words, Arch;

type
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

procedure LoadProgram(var AContext: TExecutionContext; APath: String);
var
  LProgramFile: TextFile;
  LOffset: TWord;
  LInstruction: TWord;
begin
  Assign(LProgramFile, APath);
  Reset(LProgramFile);

  while not Eof(LProgramFile) do begin
    ReadLn(LProgramFile, LOffset, LInstruction);
    AContext.Memory[LOffset] := LInstruction;
  end;

  Close(LProgramFile);
end;

var
  Context: TExecutionContext;
begin
  if ParamCount() <> 1 then begin
    WriteLn('Usage: Emulator program.t');
    Exit;
  end;

  {Write(#$1B'c');}
  InitContext(Context);
  LoadProgram(Context, ParamStr(1));
  while not Context.Halt do begin
    {Write(#$1B'[1;1H');}
    PrintContext(Context);
    ExecuteContext(Context);
  end;
end.

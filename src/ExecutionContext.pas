unit ExecutionContext;

interface

uses
  Trit, Words, Arch;

type
  TExecutionContext = record
    Registers: array[TRegister] of TWord;
    Memory: array[TWord] of TWord;
    Halt: Boolean;
  end;

procedure InitContext(var AContext: TExecutionContext);
procedure ExecuteContext(var AContext: TExecutionContext);
procedure PrintContext(var AContext: TExecutionContext);

implementation

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

procedure ExecuteContext(var AContext: TExecutionContext);
var
  LProgramCounter: TWord;

  LQuarterWords: TQuarterWordArray;
  LHalfWords: THalfWordArray;

  LOpcode: TInstructionOpcode;
  LRegD: TRegister;
  LRegA: TRegister;
  LRegB: TRegister;

  LImmediateQuarter: TQuarterWord;
  LImmediateHalf: THalfWord;
begin
  with AContext do begin
    LProgramCounter := Memory[Registers[regProgramCounter]];

    LQuarterWords := WordToQuarterWords(LProgramCounter);
    LHalfWords    := WordToHalfWords(LProgramCounter);

    LOpcode := TInstructionOpcode(LQuarterWords[3]);
    LRegD   := TRegister(LQuarterWords[2]);
    LRegA   := TRegister(LQuarterWords[1]);
    LRegB   := TRegister(LQuarterWords[0]);

    LImmediateQuarter := LQuarterWords[0];
    LImmediateHalf    := LHalfWords[0];

    case CInstructionFormats[LOpcode] of
      ifRegister:
        WriteLn(CInstructionMnemonics[LOpcode], ' ', RegisterToStr(LRegD), ' ', RegisterToStr(LRegA), ' ', RegisterToStr(LRegB));
      ifImmediate3:
        WriteLn(CInstructionMnemonics[LOpcode], ' ', RegisterToStr(LRegD), ' ', RegisterToStr(LRegA), ' ', LImmediateQuarter);
      ifImmediate6:
        WriteLn(CInstructionMnemonics[LOpcode], ' ', RegisterToStr(LRegD), ' ', LImmediateHalf);
    end;
    WriteLn();

    Registers[regProgramCounter] += 1;

    case LOpcode of
      iocUndefined13: begin
        { Temporary halt instruction }
        { Will be handled by some memory-mapped power management controller in the future }
        Halt := true;
      end;
      iocUndefined12: begin
        { Temporary debug output instruction }
        { Will be handled by some memory-mapped I/O controller in the future }
        WriteLn('Output: ', Registers[LRegD]);
        WriteLn();
      end;
      iocRotate: begin
        if LRegD <> regZero then
          Registers[LRegD] := WordRotate(Registers[LRegA], Registers[LRegB]);
      end;
      iocShiftImmediate: begin
        if LRegD <> regZero then
          Registers[LRegD] := WordShift(Registers[LRegA], LImmediateQuarter);
      end;
      iocShiftRegister: begin
        if LRegD <> regZero then
          Registers[LRegD] := WordShift(Registers[LRegA], Registers[LRegB]);
      end;
      iocNegation: begin
        if LRegD <> regZero then
          Registers[LRegD] := -Registers[LRegA];
      end;
      iocDyadicFunction: begin
        if LRegD <> regZero then
          Registers[LRegD] := WordApplyDyadicFunction(
            Registers[LRegA],
            Registers[LRegB],
            WordToDyadicFunction(Registers[LRegD]));
      end;
      iocAddRegister: begin
        if LRegD <> regZero then
          Registers[LRegD] := Registers[LRegA] + Registers[LRegB];
      end;
      iocAddImmediateQuarter: begin
        if LRegD <> regZero then
          Registers[LRegD] := Registers[LRegA] + LImmediateQuarter;
      end;
      iocAddImmediateHalf: begin
        if LRegD <> regZero then
          Registers[LRegD] += LImmediateHalf;
      end;
      iocLoadLowImmediate: begin
        if LRegD <> regZero then
          Registers[LRegD] := LImmediateHalf;
      end;
      iocLoadHighImmediate: begin
        if LRegD <> regZero then
          Registers[LRegD] := LImmediateHalf * 729; { << 6 }
      end;
      iocLoadMemory: begin
        if LRegD <> regZero then
          Registers[LRegD] := Memory[Registers[LRegA] + Registers[LRegB]];
      end;
      iocStoreMemory: begin
        Memory[Registers[LRegA] + Registers[LRegB]] := Registers[LRegD];
      end;
      iocBranchEquals: begin
        if Registers[LRegA] = Registers[LRegB] then
          Registers[regProgramCounter] := Registers[LRegD];
      end;
      iocBranchNotEquals: begin
        if Registers[LRegA] <> Registers[LRegB] then
          Registers[regProgramCounter] := Registers[LRegD];
      end;
      iocBranchLessThan: begin
        if Registers[LRegA] < Registers[LRegB] then
          Registers[regProgramCounter] := Registers[LRegD];
      end;
      iocBranchLessEqualsThan: begin
        if Registers[LRegA] <= Registers[LRegB] then
          Registers[regProgramCounter] := Registers[LRegD];
      end;
      iocPush: begin
        Memory[Registers[LRegD]] := Registers[LRegA];
        if LRegD <> regZero then
          Registers[LRegD] += 1;
      end;
      iocPull: begin
        if LRegD <> regZero then
          Registers[LRegD] -= 1;
        Registers[LRegA] := Memory[Registers[LRegD]];
      end;
      iocCall: begin
        Memory[Registers[LRegD]] := Registers[regProgramCounter];
        if LRegD <> regZero then
          Registers[LRegD] += 1;
        Registers[regProgramCounter] := Registers[LRegA];
      end;
    end;
  end;
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

end.

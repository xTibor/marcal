{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}

program Emulator;

uses
  Trit, Words, Arch, ExecutionContext;

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

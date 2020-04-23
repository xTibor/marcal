unit Utils;

interface

uses
  SysUtils;

{ Non-braindead version of the builtin mod operator }
function Modulo(AValue: LongInt; ADivisor: LongInt): LongInt;

{ The standard library has everything but a usable Split() function }
function Split(AString: String): TStringArray;

{ Chomps N characters from the start of the given string }
function ChompLeft(var AString: String; ACount: LongInt): String;

function ReadFileByLines(APath: String): TStringArray;

implementation

function Modulo(AValue: LongInt; ADivisor: LongInt): LongInt;
begin
  Modulo := AValue mod ADivisor;
  if Modulo < 0 then
    Modulo := Modulo + ADivisor;
end;

function Split(AString: String): TStringArray;
var
  LStringIndex: Integer;
  LSplitLength: Integer;
begin
  SetLength(Split, 0);
  LStringIndex := 1;
  LSplitLength := 0;

  while True do begin
    while (LStringIndex <= Length(AString)) and (AString[LStringIndex] = ' ') do
      LStringIndex += 1;

    if LStringIndex > Length(AString) then
      Exit;

    LSplitLength += 1;
    SetLength(Split, LSplitLength);

    while (LStringIndex <= Length(AString)) and (AString[LStringIndex] <> ' ') do begin
      Split[LSplitLength - 1] += AString[LStringIndex];
      LStringIndex += 1;
    end;
  end;
end;

function ChompLeft(var AString: String; ACount: LongInt): String;
begin
  ChompLeft := Copy(AString, 1, ACount);
  Delete(AString, 1, ACount);
end;

function ReadFileByLines(APath: String): TStringArray;
var
  LFile: TextFile;
begin
  SetLength(ReadFileByLines, 0);

  Assign(LFile, APath);
  Reset(LFile);
  while not Eof(LFile) do begin
    SetLength(ReadFileByLines, Length(ReadFileByLines) + 1);
    ReadLn(LFile, ReadFileByLines[Length(ReadFileByLines) - 1]);
  end;
  Close(LFile);
end;

end.

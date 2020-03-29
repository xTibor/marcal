unit Utils;

interface

uses
  SysUtils;

{ Non-braindead version of the builtin mod operator }
function Modulo(AValue: LongInt; ADivisor: LongInt): LongInt;

{ The standard library has everything but a usable Split() function }
function Split(AString: String): TStringArray;

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

end.

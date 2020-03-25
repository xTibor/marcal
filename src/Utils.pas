unit Utils;

interface

{ Non-braindead version of the builtin mod operator }
function Modulo(AValue: LongInt; ADivisor: LongInt): LongInt;

implementation

function Modulo(AValue: LongInt; ADivisor: LongInt): LongInt;
begin
  Modulo := AValue mod ADivisor;
  if Modulo < 0 then
    Modulo := Modulo + ADivisor;
end;

end.

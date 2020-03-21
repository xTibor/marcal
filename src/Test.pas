unit Test;

interface

procedure Test;

implementation

uses
  Trit, Tryte;

procedure Test;
var
  LongTryte: TLongTryte;
  ShortTryte: TShortTryte;
  Trits: TTryteTrits;
  SplicedShort: TShortSplice;
  SplicedHalf: THalfSplice;
begin
  for LongTryte := -100 to 100 do
    WriteLn(LongTryte:4, ': ', LongTryteToStr(LongTryte));

  for ShortTryte := -13 to 13 do
    WriteLn(ShortTryte:4, ': ', ShortTryteToStr(ShortTryte));

  for LongTryte := -90000 to -89970 do begin
    WriteLn(LongTryte:4, ': ', LongTryteToStr(LongTryte));

    SplicedShort := LongTryteShortSplice(LongTryte);
    WriteLn(
      'Opcd: ', ShortTryteToStr(SplicedShort[0]), ', ',
      'RegA: ', ShortTryteToStr(SplicedShort[1]), ', ',
      'RegB: ', ShortTryteToStr(SplicedShort[2]), ', ',
      'RegD: ', ShortTryteToStr(SplicedShort[3]));

    SplicedHalf := LongTryteHalfSplice(LongTryte);
    WriteLn(
      'Lo: ', HalfTryteToStr(SplicedHalf[0]), ', ',
      'Hi: ', HalfTryteToStr(SplicedHalf[1]));

  end;

  Trits := TTryteTrits.Create(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);
  WriteLn(LongTryteEncode(Trits));

  WriteLn(LongTryteToStr(265720));
  WriteLn(LongTryteToStr(-265720));
end;

end.

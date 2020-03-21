unit Tryte;

interface

uses
  Trit;

{ Generic tryte }

type
  TTryte = LongInt;
  TTryteTrits = array of TTrit;

function TryteEncode(ATryteBits: TTryteTrits; ATritCount: Integer): TTryte;
function TryteDecode(ATryte: TTryte; ATritCount: Integer): TTryteTrits;
function TryteToStr(ATryte: TTryte; ATritCount: Integer): String;

{ Short tryte }

const
  CShortTryteTritCount = 3;
type
  TShortTryte = -13..13;

function ShortTryteEncode(ATryteBits: TTryteTrits): TShortTryte;
function ShortTryteDecode(ATryte: TShortTryte): TTryteTrits;
function ShortTryteToStr(ATryte: TShortTryte): String;

{ Half tryte }

const
  CHalfTryteTritCount = 6;
type
  THalfTryte = -364..364;

function HalfTryteEncode(ATryteBits: TTryteTrits): THalfTryte;
function HalfTryteDecode(ATryte: THalfTryte): TTryteTrits;
function HalfTryteToStr(ATryte: THalfTryte): String;

{ Long tryte }

const
  CLongTryteTritCount = 12;
type
  TLongTryte = -265720..265720;

function LongTryteEncode(ATryteBits: TTryteTrits): TLongTryte;
function LongTryteDecode(ATryte: TLongTryte): TTryteTrits;
function LongTryteToStr(ATryte: TLongTryte): String;

type
  TShortSplice = array[0..3] of TShortTryte;
  THalfSplice = array[0..1] of THalfTryte;

function LongTryteShortSplice(ATryte: TLongTryte): TShortSplice;
function LongTryteHalfSplice(ATryte: TLongTryte): THalfSplice;

implementation

{ Generic tryte }

function TryteEncode(ATryteBits: TTryteTrits; ATritCount: Integer): TTryte;
var
  LIndex: Integer;
  LPower: LongInt;
begin
  TryteEncode := 0;
  LPower := 1;
  for LIndex := 0 to ATritCount - 1 do begin
    TryteEncode += ATryteBits[LIndex] * LPower;
    LPower *= 3;
  end;
end;

function TryteDecode(ATryte: TTryte; ATritCount: Integer): TTryteTrits;
var
  LNegative: Boolean;
  LIndex: Integer;
  LBalancedTrit: Integer;
begin
  LNegative := ATryte < 0;
  ATryte := Abs(ATryte);
  SetLength(TryteDecode, ATritCount);

  for LIndex := 0 to ATritCount - 1 do begin
    LBalancedTrit := ATryte mod 3;
    ATryte := ATryte div 3;

    if LBalancedTrit = 2 then begin
      TryteDecode[LIndex] := -1;
      ATryte += 1;
    end else
      TryteDecode[LIndex] := LBalancedTrit;
  end;

  if LNegative then begin
    for LIndex := 0 to ATritCount - 1 do
      TryteDecode[LIndex] := CTritOpInvert[TryteDecode[LIndex]];
  end;
end;

function TryteToStr(ATryte: TTryte; ATritCount: Integer): String;
var
  LIndex: Integer;
  LTrits: TTryteTrits;
begin
  LTrits := TryteDecode(ATryte, ATritCount);

  TryteToStr := '[';
  for LIndex := ATritCount - 1 downto 0 do
    TryteToStr += CTritToStr[LTrits[LIndex]];
  TryteToStr += ']';
end;

{ Short tryte }

function ShortTryteEncode(ATryteBits: TTryteTrits): TShortTryte;
begin
  ShortTryteEncode := TryteEncode(ATryteBits, CShortTryteTritCount);
end;

function ShortTryteDecode(ATryte: TShortTryte): TTryteTrits;
begin
  ShortTryteDecode := TryteDecode(ATryte, CShortTryteTritCount);
end;

function ShortTryteToStr(ATryte: TShortTryte): String;
begin
  ShortTryteToStr := TryteToStr(ATryte, CShortTryteTritCount);
end;

{ Half tryte }

function HalfTryteEncode(ATryteBits: TTryteTrits): THalfTryte;
begin
  HalfTryteEncode := TryteEncode(ATryteBits, CHalfTryteTritCount);
end;

function HalfTryteDecode(ATryte: THalfTryte): TTryteTrits;
begin
  HalfTryteDecode := TryteDecode(ATryte, CHalfTryteTritCount);
end;

function HalfTryteToStr(ATryte: THalfTryte): String;
begin
  HalfTryteToStr := TryteToStr(ATryte, CHalfTryteTritCount);
end;

{ Long tryte }

function LongTryteEncode(ATryteBits: TTryteTrits): TLongTryte;
begin
  LongTryteEncode := TryteEncode(ATryteBits, CLongTryteTritCount);
end;

function LongTryteDecode(ATryte: TLongTryte): TTryteTrits;
begin
  LongTryteDecode := TryteDecode(ATryte, CLongTryteTritCount);
end;

function LongTryteToStr(ATryte: TLongTryte): String;
begin
  LongTryteToStr := TryteToStr(ATryte, CLongTryteTritCount);
end;

function LongTryteShortSplice(ATryte: TLongTryte): TShortSplice;
var
  LIndex: Integer;
  LTrits: TTryteTrits;
begin
  LTrits := LongTryteDecode(ATryte);
  for LIndex := Low(TShortSplice) to High(TShortSplice) do
    LongTryteShortSplice[LIndex] := ShortTryteEncode(
      Copy(LTrits, LIndex * CShortTryteTritCount, CShortTryteTritCount));
end;

function LongTryteHalfSplice(ATryte: TLongTryte): THalfSplice;
var
  LIndex: Integer;
  LTrits: TTryteTrits;
begin
  LTrits := LongTryteDecode(ATryte);
  for LIndex := Low(THalfSplice) to High(THalfSplice) do
    LongTryteHalfSplice[LIndex] := HalfTryteEncode(
      Copy(LTrits, LIndex * CHalfTryteTritCount, CHalfTryteTritCount));
end;

end.

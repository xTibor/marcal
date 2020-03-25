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
function TryteApplyMonadicFunction(ATryte: TTryte; ATritCount: Integer; AFunction: TTritMonadicFunction): TTryte;
function TryteApplyDyadicFunction(ATryteA: TTryte; ATryteB: TTryte; ATritCount: Integer; AFunction: TTritDyadicFunction): TTryte;
function TryteShift(ATryte: TTryte; ATritCount: Integer; AShift: Integer): TTryte;
function TryteRotate(ATryte: TTryte; ATritCount: Integer; AShift: Integer): TTryte;

{ Short tryte }

const
  CShortTryteTritCount = 3;
type
  TShortTryte = -13..13;

function ShortTryteEncode(ATryteBits: TTryteTrits): TShortTryte;
function ShortTryteDecode(ATryte: TShortTryte): TTryteTrits;
function ShortTryteToStr(ATryte: TShortTryte): String;
function ShortTryteApplyMonadicFunction(ATryte: TShortTryte; AFunction: TTritMonadicFunction): TShortTryte;
function ShortTryteApplyDyadicFunction(ATryteA: TShortTryte; ATryteB: TShortTryte; AFunction: TTritDyadicFunction): TShortTryte;
function ShortTryteShift(ATryte: TShortTryte; AShift: Integer): TShortTryte;
function ShortTryteRotate(ATryte: TShortTryte; AShift: Integer): TShortTryte;

{ Half tryte }

const
  CHalfTryteTritCount = 6;
type
  THalfTryte = -364..364;

function HalfTryteEncode(ATryteBits: TTryteTrits): THalfTryte;
function HalfTryteDecode(ATryte: THalfTryte): TTryteTrits;
function HalfTryteToStr(ATryte: THalfTryte): String;
function HalfTryteApplyMonadicFunction(ATryte: THalfTryte; AFunction: TTritMonadicFunction): THalfTryte;
function HalfTryteApplyDyadicFunction(ATryteA: THalfTryte; ATryteB: THalfTryte; AFunction: TTritDyadicFunction): THalfTryte;
function HalfTryteShift(ATryte: THalfTryte; AShift: Integer): THalfTryte;
function HalfTryteRotate(ATryte: THalfTryte; AShift: Integer): THalfTryte;

{ Long tryte }

const
  CLongTryteTritCount = 12;
type
  TLongTryte = -265720..265720;

function LongTryteEncode(ATryteBits: TTryteTrits): TLongTryte;
function LongTryteDecode(ATryte: TLongTryte): TTryteTrits;
function LongTryteToStr(ATryte: TLongTryte): String;
function LongTryteApplyMonadicFunction(ATryte: TLongTryte; AFunction: TTritMonadicFunction): TLongTryte;
function LongTryteApplyDyadicFunction(ATryteA: TLongTryte; ATryteB: TLongTryte; AFunction: TTritDyadicFunction): TLongTryte;
function LongTryteShift(ATryte: TLongTryte; AShift: Integer): TLongTryte;
function LongTryteRotate(ATryte: TLongTryte; AShift: Integer): TLongTryte;

type
  TShortSplice = array[0..3] of TShortTryte;
  THalfSplice = array[0..1] of THalfTryte;

function LongTryteShortSplice(ATryte: TLongTryte): TShortSplice;
function LongTryteHalfSplice(ATryte: TLongTryte): THalfSplice;
function LongTryteToDyadicFunction(ATryte: TLongTryte): TTritDyadicFunction;

implementation

uses
  Utils;

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
      TryteDecode[LIndex] := CTritFunctionNegation[TryteDecode[LIndex]];
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

function TryteApplyMonadicFunction(ATryte: TTryte; ATritCount: Integer; AFunction: TTritMonadicFunction): TTryte;
var
  LIndex: Integer;
  LTrits: TTryteTrits;
begin
  LTrits := TryteDecode(ATryte, ATritCount);

  for LIndex := 0 to ATritCount - 1 do
    LTrits[LIndex] := AFunction[LTrits[LIndex]];

  TryteApplyMonadicFunction := TryteEncode(LTrits, ATritCount);
end;

function TryteApplyDyadicFunction(ATryteA: TTryte; ATryteB: TTryte; ATritCount: Integer; AFunction: TTritDyadicFunction): TTryte;
var
  LIndex: Integer;
  LTritsA, LTritsB: TTryteTrits;
  LTritsResult: TTryteTrits;
begin
  LTritsA := TryteDecode(ATryteA, ATritCount);
  LTritsB := TryteDecode(ATryteB, ATritCount);
  SetLength(LTritsResult, ATritCount);

  for LIndex := 0 to ATritCount - 1 do
    LTritsResult[LIndex] := AFunction[LTritsA[LIndex], LTritsB[LIndex]];

  TryteApplyDyadicFunction := TryteEncode(LTritsResult, ATritCount);
end;

function TryteShift(ATryte: TTryte; ATritCount: Integer; AShift: Integer): TTryte;
var
  LIndex: Integer;
  LShiftedIndex: Integer;
  LTrits: TTryteTrits;
  LTritsResult: TTryteTrits;
begin
  if (AShift > ATritCount) or (AShift < -ATritCount) then begin
    TryteShift := 0;
  end else begin
    LTrits := TryteDecode(ATryte, ATritCount);
    SetLength(LTritsResult, ATritCount);

    for LIndex := 0 to ATritCount - 1 do begin
      LShiftedIndex := LIndex - AShift;
      if (LShiftedIndex >= 0) and (LShiftedIndex < ATritCount) then
        LTritsResult[LIndex] := LTrits[LShiftedIndex]
      else
        LTritsResult[LIndex] := 0;
    end;

    TryteShift := TryteEncode(LTritsResult, ATritCount);
  end;
end;

function TryteRotate(ATryte: TTryte; ATritCount: Integer; AShift: Integer): TTryte;
var
  LIndex: Integer;
  LTrits: TTryteTrits;
  LTritsResult: TTryteTrits;
begin
  if (AShift > ATritCount) or (AShift < -ATritCount) then begin
    TryteRotate := 0;
  end else begin
    LTrits := TryteDecode(ATryte, ATritCount);
    SetLength(LTritsResult, ATritCount);

    for LIndex := 0 to ATritCount - 1 do
      LTritsResult[LIndex] := LTrits[Modulo(LIndex - AShift, ATritCount)];

    TryteRotate := TryteEncode(LTritsResult, ATritCount);
  end;
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

function ShortTryteApplyMonadicFunction(ATryte: TShortTryte; AFunction: TTritMonadicFunction): TShortTryte;
begin
  ShortTryteApplyMonadicFunction := TryteApplyMonadicFunction(ATryte, CShortTryteTritCount, AFunction);
end;

function ShortTryteApplyDyadicFunction(ATryteA: TShortTryte; ATryteB: TShortTryte; AFunction: TTritDyadicFunction): TShortTryte;
begin
  ShortTryteApplyDyadicFunction := TryteApplyDyadicFunction(ATryteA, ATryteB, CShortTryteTritCount, AFunction);
end;

function ShortTryteShift(ATryte: TShortTryte; AShift: Integer): TShortTryte;
begin
  ShortTryteShift := TryteShift(ATryte, CShortTryteTritCount, AShift);
end;

function ShortTryteRotate(ATryte: TShortTryte; AShift: Integer): TShortTryte;
begin
  ShortTryteRotate := TryteRotate(ATryte, CShortTryteTritCount, AShift);
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

function HalfTryteApplyMonadicFunction(ATryte: THalfTryte; AFunction: TTritMonadicFunction): THalfTryte;
begin
  HalfTryteApplyMonadicFunction := TryteApplyMonadicFunction(ATryte, CHalfTryteTritCount, AFunction);
end;

function HalfTryteApplyDyadicFunction(ATryteA: THalfTryte; ATryteB: THalfTryte; AFunction: TTritDyadicFunction): THalfTryte;
begin
  HalfTryteApplyDyadicFunction := TryteApplyDyadicFunction(ATryteA, ATryteB, CHalfTryteTritCount, AFunction);
end;

function HalfTryteShift(ATryte: THalfTryte; AShift: Integer): THalfTryte;
begin
  HalfTryteShift := TryteShift(ATryte, CHalfTryteTritCount, AShift);
end;

function HalfTryteRotate(ATryte: THalfTryte; AShift: Integer): THalfTryte;
begin
  HalfTryteRotate := TryteRotate(ATryte, CHalfTryteTritCount, AShift);
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

function LongTryteApplyMonadicFunction(ATryte: TLongTryte; AFunction: TTritMonadicFunction): TLongTryte;
begin
  LongTryteApplyMonadicFunction := TryteApplyMonadicFunction(ATryte, CLongTryteTritCount, AFunction);
end;

function LongTryteApplyDyadicFunction(ATryteA: TLongTryte; ATryteB: TLongTryte; AFunction: TTritDyadicFunction): TLongTryte;
begin
  LongTryteApplyDyadicFunction := TryteApplyDyadicFunction(ATryteA, ATryteB, CLongTryteTritCount, AFunction);
end;

function LongTryteShift(ATryte: TLongTryte; AShift: Integer): TLongTryte;
begin
  LongTryteShift := TryteShift(ATryte, CLongTryteTritCount, AShift);
end;

function LongTryteRotate(ATryte: TLongTryte; AShift: Integer): TLongTryte;
begin
  LongTryteRotate := TryteRotate(ATryte, CLongTryteTritCount, AShift);
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

function LongTryteToDyadicFunction(ATryte: TLongTryte): TTritDyadicFunction;
var
  LIndex: Integer;
  LTrits: TTryteTrits;
  LTritA, LTritB: TTrit;
begin
  LTrits := LongTryteDecode(ATryte);

  LIndex := 0;
  for LTritA := Low(TTrit) to High(TTrit) do begin
    for LTritB := Low(TTrit) to High(TTrit) do begin
      LongTryteToDyadicFunction[LTritA, LTritB] := LTrits[LIndex];
      LIndex := LIndex + 1;
    end;
  end;
end;

end.

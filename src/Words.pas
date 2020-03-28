unit Words;

interface

uses
  Trit;

{ Word data type }

const
  CWordTritCount = 12;
type
  TWord = -265720..265720;

function WordEncode(ATrits: TTritArray): TWord;
function WordDecode(AWord: TWord): TTritArray;
function WordToStr(AWord: TWord): String;
function WordApplyMonadicFunction(AWord: TWord; AFunction: TTritMonadicFunction): TWord;
function WordApplyDyadicFunction(ALeftWord: TWord; ARightWord: TWord; AFunction: TTritDyadicFunction): TWord;
function WordShift(AWord: TWord; AShift: Integer): TWord;
function WordRotate(AWord: TWord; AShift: Integer): TWord;

{ Half-word data type }

const
  CHalfWordTritCount = 6;
type
  THalfWord = -364..364;

function HalfWordEncode(ATrits: TTritArray): THalfWord;
function HalfWordDecode(AWord: THalfWord): TTritArray;
function HalfWordToStr(AWord: THalfWord): String;
function HalfWordApplyMonadicFunction(AWord: THalfWord; AFunction: TTritMonadicFunction): THalfWord;
function HalfWordApplyDyadicFunction(ALeftWord: THalfWord; ARightWord: THalfWord; AFunction: TTritDyadicFunction): THalfWord;
function HalfWordShift(AWord: THalfWord; AShift: Integer): THalfWord;
function HalfWordRotate(AWord: THalfWord; AShift: Integer): THalfWord;

{ Quarter-word data type }

const
  CQuarterWordTritCount = 3;
type
  TQuarterWord = -13..13;

function QuarterWordEncode(ATrits: TTritArray): TQuarterWord;
function QuarterWordDecode(AWord: TQuarterWord): TTritArray;
function QuarterWordToStr(AWord: TQuarterWord): String;
function QuarterWordApplyMonadicFunction(AWord: TQuarterWord; AFunction: TTritMonadicFunction): TQuarterWord;
function QuarterWordApplyDyadicFunction(ALeftWord: TQuarterWord; ARightWord: TQuarterWord; AFunction: TTritDyadicFunction): TQuarterWord;
function QuarterWordShift(AWord: TQuarterWord; AShift: Integer): TQuarterWord;
function QuarterWordRotate(AWord: TQuarterWord; AShift: Integer): TQuarterWord;

{ Conversion utils }

type
  TQuarterWordArray = array[0..3] of TQuarterWord;
  THalfWordArray = array[0..1] of THalfWord;

function WordToQuarterWords(AWord: TWord): TQuarterWordArray;
function WordToHalfWords(AWord: TWord): THalfWordArray;
function WordToDyadicFunction(AWord: TWord): TTritDyadicFunction;

implementation

uses
  Utils;

{ Generic word functions }

type
  TGenericWord = LongInt;

function GenericEncode(ATrits: TTritArray; ATritCount: Integer): TGenericWord;
var
  LIndex: Integer;
  LPower: LongInt;
begin
  GenericEncode := 0;
  LPower := 1;
  for LIndex := 0 to ATritCount - 1 do begin
    GenericEncode += ATrits[LIndex] * LPower;
    LPower *= 3;
  end;
end;

function GenericDecode(AWord: TGenericWord; ATritCount: Integer): TTritArray;
var
  LNegative: Boolean;
  LIndex: Integer;
  LBalancedTrit: Integer;
begin
  LNegative := AWord < 0;
  AWord := Abs(AWord);
  SetLength(GenericDecode, ATritCount);

  for LIndex := 0 to ATritCount - 1 do begin
    LBalancedTrit := AWord mod 3;
    AWord := AWord div 3;

    if LBalancedTrit = 2 then begin
      GenericDecode[LIndex] := -1;
      AWord += 1;
    end else
      GenericDecode[LIndex] := LBalancedTrit;
  end;

  if LNegative then begin
    for LIndex := 0 to ATritCount - 1 do
      GenericDecode[LIndex] := CTritFunctionNegation[GenericDecode[LIndex]];
  end;
end;

function GenericToStr(AWord: TGenericWord; ATritCount: Integer): String;
var
  LIndex: Integer;
  LTrits: TTritArray;
begin
  LTrits := GenericDecode(AWord, ATritCount);

  GenericToStr := '[';
  for LIndex := ATritCount - 1 downto 0 do
    GenericToStr += CTritToStr[LTrits[LIndex]];
  GenericToStr += ']';
end;

function GenericApplyMonadicFunction(AWord: TGenericWord; ATritCount: Integer; AFunction: TTritMonadicFunction): TGenericWord;
var
  LIndex: Integer;
  LTrits: TTritArray;
begin
  LTrits := GenericDecode(AWord, ATritCount);

  for LIndex := 0 to ATritCount - 1 do
    LTrits[LIndex] := AFunction[LTrits[LIndex]];

  GenericApplyMonadicFunction := GenericEncode(LTrits, ATritCount);
end;

function GenericApplyDyadicFunction(ALeftWord: TGenericWord; ARightWord: TGenericWord; ATritCount: Integer; AFunction: TTritDyadicFunction): TGenericWord;
var
  LIndex: Integer;
  LTritsA, LTritsB: TTritArray;
  LTritsResult: TTritArray;
begin
  LTritsA := GenericDecode(ALeftWord, ATritCount);
  LTritsB := GenericDecode(ARightWord, ATritCount);
  SetLength(LTritsResult, ATritCount);

  for LIndex := 0 to ATritCount - 1 do
    LTritsResult[LIndex] := AFunction[LTritsA[LIndex], LTritsB[LIndex]];

  GenericApplyDyadicFunction := GenericEncode(LTritsResult, ATritCount);
end;

function GenericShift(AWord: TGenericWord; ATritCount: Integer; AShift: Integer): TGenericWord;
var
  LIndex: Integer;
  LShiftedIndex: Integer;
  LTrits: TTritArray;
  LTritsResult: TTritArray;
begin
  if (AShift > ATritCount) or (AShift < -ATritCount) then begin
    GenericShift := 0;
  end else begin
    LTrits := GenericDecode(AWord, ATritCount);
    SetLength(LTritsResult, ATritCount);

    for LIndex := 0 to ATritCount - 1 do begin
      LShiftedIndex := LIndex - AShift;
      if (LShiftedIndex >= 0) and (LShiftedIndex < ATritCount) then
        LTritsResult[LIndex] := LTrits[LShiftedIndex]
      else
        LTritsResult[LIndex] := 0;
    end;

    GenericShift := GenericEncode(LTritsResult, ATritCount);
  end;
end;

function GenericRotate(AWord: TGenericWord; ATritCount: Integer; AShift: Integer): TGenericWord;
var
  LIndex: Integer;
  LTrits: TTritArray;
  LTritsResult: TTritArray;
begin
  if (AShift > ATritCount) or (AShift < -ATritCount) then begin
    GenericRotate := 0;
  end else begin
    LTrits := GenericDecode(AWord, ATritCount);
    SetLength(LTritsResult, ATritCount);

    for LIndex := 0 to ATritCount - 1 do
      LTritsResult[LIndex] := LTrits[Modulo(LIndex - AShift, ATritCount)];

    GenericRotate := GenericEncode(LTritsResult, ATritCount);
  end;
end;

{ Word data type }

function WordEncode(ATrits: TTritArray): TWord;
begin
  WordEncode := GenericEncode(ATrits, CWordTritCount);
end;

function WordDecode(AWord: TWord): TTritArray;
begin
  WordDecode := GenericDecode(AWord, CWordTritCount);
end;

function WordToStr(AWord: TWord): String;
begin
  WordToStr := GenericToStr(AWord, CWordTritCount);
end;

function WordApplyMonadicFunction(AWord: TWord; AFunction: TTritMonadicFunction): TWord;
begin
  WordApplyMonadicFunction := GenericApplyMonadicFunction(AWord, CWordTritCount, AFunction);
end;

function WordApplyDyadicFunction(ALeftWord: TWord; ARightWord: TWord; AFunction: TTritDyadicFunction): TWord;
begin
  WordApplyDyadicFunction := GenericApplyDyadicFunction(ALeftWord, ARightWord, CWordTritCount, AFunction);
end;

function WordShift(AWord: TWord; AShift: Integer): TWord;
begin
  WordShift := GenericShift(AWord, CWordTritCount, AShift);
end;

function WordRotate(AWord: TWord; AShift: Integer): TWord;
begin
  WordRotate := GenericRotate(AWord, CWordTritCount, AShift);
end;

{ Half-word data type }

function HalfWordEncode(ATrits: TTritArray): THalfWord;
begin
  HalfWordEncode := GenericEncode(ATrits, CHalfWordTritCount);
end;

function HalfWordDecode(AWord: THalfWord): TTritArray;
begin
  HalfWordDecode := GenericDecode(AWord, CHalfWordTritCount);
end;

function HalfWordToStr(AWord: THalfWord): String;
begin
  HalfWordToStr := GenericToStr(AWord, CHalfWordTritCount);
end;

function HalfWordApplyMonadicFunction(AWord: THalfWord; AFunction: TTritMonadicFunction): THalfWord;
begin
  HalfWordApplyMonadicFunction := GenericApplyMonadicFunction(AWord, CHalfWordTritCount, AFunction);
end;

function HalfWordApplyDyadicFunction(ALeftWord: THalfWord; ARightWord: THalfWord; AFunction: TTritDyadicFunction): THalfWord;
begin
  HalfWordApplyDyadicFunction := GenericApplyDyadicFunction(ALeftWord, ARightWord, CHalfWordTritCount, AFunction);
end;

function HalfWordShift(AWord: THalfWord; AShift: Integer): THalfWord;
begin
  HalfWordShift := GenericShift(AWord, CHalfWordTritCount, AShift);
end;

function HalfWordRotate(AWord: THalfWord; AShift: Integer): THalfWord;
begin
  HalfWordRotate := GenericRotate(AWord, CHalfWordTritCount, AShift);
end;

{ Quarter-word data type }

function QuarterWordEncode(ATrits: TTritArray): TQuarterWord;
begin
  QuarterWordEncode := GenericEncode(ATrits, CQuarterWordTritCount);
end;

function QuarterWordDecode(AWord: TQuarterWord): TTritArray;
begin
  QuarterWordDecode := GenericDecode(AWord, CQuarterWordTritCount);
end;

function QuarterWordToStr(AWord: TQuarterWord): String;
begin
  QuarterWordToStr := GenericToStr(AWord, CQuarterWordTritCount);
end;

function QuarterWordApplyMonadicFunction(AWord: TQuarterWord; AFunction: TTritMonadicFunction): TQuarterWord;
begin
  QuarterWordApplyMonadicFunction := GenericApplyMonadicFunction(AWord, CQuarterWordTritCount, AFunction);
end;

function QuarterWordApplyDyadicFunction(ALeftWord: TQuarterWord; ARightWord: TQuarterWord; AFunction: TTritDyadicFunction): TQuarterWord;
begin
  QuarterWordApplyDyadicFunction := GenericApplyDyadicFunction(ALeftWord, ARightWord, CQuarterWordTritCount, AFunction);
end;

function QuarterWordShift(AWord: TQuarterWord; AShift: Integer): TQuarterWord;
begin
  QuarterWordShift := GenericShift(AWord, CQuarterWordTritCount, AShift);
end;

function QuarterWordRotate(AWord: TQuarterWord; AShift: Integer): TQuarterWord;
begin
  QuarterWordRotate := GenericRotate(AWord, CQuarterWordTritCount, AShift);
end;

{ Conversion utils }

function WordToQuarterWords(AWord: TWord): TQuarterWordArray;
var
  LIndex: Integer;
  LTrits: TTritArray;
begin
  LTrits := WordDecode(AWord);
  for LIndex := Low(TQuarterWordArray) to High(TQuarterWordArray) do
    WordToQuarterWords[LIndex] := QuarterWordEncode(
      Copy(LTrits, LIndex * CQuarterWordTritCount, CQuarterWordTritCount));
end;

function WordToHalfWords(AWord: TWord): THalfWordArray;
var
  LIndex: Integer;
  LTrits: TTritArray;
begin
  LTrits := WordDecode(AWord);
  for LIndex := Low(THalfWordArray) to High(THalfWordArray) do
    WordToHalfWords[LIndex] := HalfWordEncode(
      Copy(LTrits, LIndex * CHalfWordTritCount, CHalfWordTritCount));
end;

function WordToDyadicFunction(AWord: TWord): TTritDyadicFunction;
var
  LIndex: Integer;
  LTrits: TTritArray;
  LTritA, LTritB: TTrit;
begin
  LTrits := WordDecode(AWord);

  LIndex := 0;
  for LTritA := Low(TTrit) to High(TTrit) do begin
    for LTritB := Low(TTrit) to High(TTrit) do begin
      WordToDyadicFunction[LTritA, LTritB] := LTrits[LIndex];
      LIndex := LIndex + 1;
    end;
  end;
end;

end.

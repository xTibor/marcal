unit Trit;

interface

type
  TTrit = -1..1;

const
  CTritToStr: array[TTrit] of Char = ('-', '0', '+');

type
  TTritArray = array of TTrit;

{ Monadic functions }

type
  TTritMonadicFunction = array[TTrit] of TTrit;

const
  CTritFunctionIdentity:    TTritMonadicFunction = (-1,  0,  1);
  CTritFunctionNegation:    TTritMonadicFunction = ( 1,  0, -1);
  CTritFunctionIncrement:   TTritMonadicFunction = ( 0,  1, -1);
  CTritFunctionDecrement:   TTritMonadicFunction = ( 1, -1,  0);
  CTritFunctionSelectMinus: TTritMonadicFunction = ( 1, -1, -1);
  CTritFunctionSelectZero:  TTritMonadicFunction = (-1,  1, -1);
  CTritFunctionSelectPlus:  TTritMonadicFunction = (-1, -1,  1);

{ Dyadic functions }

type
  TTritDyadicFunction = array[TTrit, TTrit] of TTrit;

const
  CTritFunctionAnd: TTritDyadicFunction = (
    (-1, -1, -1),
    (-1,  0,  0),
    (-1,  0,  1)
  );

  CTritFunctionOr: TTritDyadicFunction = (
    (-1,  0,  1),
    ( 0,  0,  1),
    ( 1,  1,  1)
  );

  CTritFunctionAddSum: TTritDyadicFunction = (
    ( 1, -1,  0),
    (-1,  0,  1),
    ( 0,  1, -1)
  );

  CTritFunctionAddCarry: TTritDyadicFunction = (
    (-1,  0,  0),
    ( 0,  0,  0),
    ( 0,  0,  1)
  );

  CTritFunctionEquality: TTritDyadicFunction = (
    ( 1, -1, -1),
    (-1,  1, -1),
    (-1, -1,  1)
  );

implementation

end.

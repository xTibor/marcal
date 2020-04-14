unit Trit;

interface

type
  TTrit = -1..1;

const
  CTritToStr: array[TTrit] of Char = ('-', '0', '+');

type
  TTritArray = array of TTrit;

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

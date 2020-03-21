unit Trit;

interface

type
  TTrit = -1..1;

const
  CTritToStr: array[TTrit] of Char = ('-', '0', '+');

{ Monadic operations }

type
  TTritMonadicOp = array[TTrit] of TTrit;

const
  CTritOpIdentity:    TTritMonadicOp = (-1,  0,  1);
  CTritOpInvert:      TTritMonadicOp = ( 1,  0, -1);
  CTritOpIncrement:   TTritMonadicOp = ( 0,  1, -1);
  CTritOpDecrement:   TTritMonadicOp = ( 1, -1,  0);
  CTritOpSelectMinus: TTritMonadicOp = ( 1, -1, -1);
  CTritOpSelectZero:  TTritMonadicOp = (-1,  1, -1);
  CTritOpSelectPlus:  TTritMonadicOp = (-1, -1,  1);

{ Dyadic operations }

type
  TTritDyadicOp = array[TTrit, TTrit] of TTrit;

const
  CTritOpAnd: TTritDyadicOp = (
    (-1, -1, -1),
    (-1,  0,  0),
    (-1,  0,  1)
  );

  CTritOpOr: TTritDyadicOp = (
    (-1,  0,  1),
    ( 0,  0,  1),
    ( 1,  1,  1)
  );

  CTritOpAddSum: TTritDyadicOp = (
    ( 1, -1,  0),
    (-1,  0,  1),
    ( 0,  1, -1)
  );

  CTritOpAddCarry: TTritDyadicOp = (
    (-1,  0,  0),
    ( 0,  0,  0),
    ( 0,  0,  1)
  );

  CTritOpEquality: TTritDyadicOp = (
    ( 1, -1, -1),
    (-1,  1, -1),
    (-1, -1,  1)
  );

implementation

end.

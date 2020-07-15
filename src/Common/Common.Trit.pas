unit Common.Trit;

interface

type
  TTrit = -1..1;

const
  CTritToStr: array[TTrit] of Char = ('-', '0', '+');

type
  TTritArray = array of TTrit;
  TTritDyadicFunction = array[TTrit, TTrit] of TTrit;

implementation

end.

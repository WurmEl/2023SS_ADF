program test;

type
  IntArray = array [1..1] of INTEGER;
var
  ap: ^IntArray; (* array pointer = pointer to dynamic array *)
  n, i: INTEGER;

begin
  n := 3; (* size of array *)
  GetMem(ap, n * SizeOf(INTEGER));
  if ap = NIL then halt(1);
    {$R-}
  for i := 1 to n do ap^[i] := 0;
  for i := 1 to n do writeln(ap^[i]);
    {$R+}

  FreeMem(ap, n * SizeOf(INTEGER));
end.

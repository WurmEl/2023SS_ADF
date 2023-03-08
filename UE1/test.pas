(* ArrayTest:                                                 Daniel Mitterlehner, 2022-11-30 *)
(* ------                                                                    *)
(* Three different ways of array implementation:
   (1) array of fixed size in static memory
   (2) array of fixed size on the heap
   (3) array of "dynamic" size on the heap                            *)
(* ========================================================================= *)

PROGRAM ArrayTest;

  CONST
    max = 100;

  TYPE
    IntArray = ARRAY [0..max-1] OF INTEGER; (* fixed length, static mem *)
    DynamicIntArray = ARRAY [0..0] OF INTEGER; (* flexible length *)

  VAR
    a: IntArray;
    ap: ^IntArray;          (* array pointer *)
    dap: ^DynamicIntArray;  (* dynamic array pointer *)
    n,i,size: INTEGER;
BEGIN (* ArrayTest *)
  
  n := 20;
  (* v1 *)
  FOR i := 0 TO n-1 DO BEGIN
    a[i] := i;      (* set *)
    Write(a[i]:3);  (* get *)
  END; (* FOR *)
  WriteLn;

  (* v2 *)
  New(ap);
  FOR i := 0 TO n-1 DO BEGIN
    ap^[i] := i;      (* set *)
    Write(ap^[i]:3);  (* get *)
  END; (* FOR *)
  WriteLn;
  Dispose(ap);

  (* v3 *)
  Write('number of elements? > '); ReadLn(n);
  size := n * SizeOf(INTEGER);
  GetMem(dap, size);
  FOR i := 0 TO n-1 DO BEGIN
    (*$R-*)
    dap^[i] := i;      (* set *)
    Write(dap^[i]:3);  (* get *)
    (*$R+*)
  END; (* FOR *)
  FreeMem(dap, size);


END. (* ArrayTest *)
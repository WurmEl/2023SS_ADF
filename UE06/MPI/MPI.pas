(* MPI:                                                      SWa, 2023-05-03 *)
(* ----                                                                      *)
(* MiniPascal interpreter main program.                                      *)
(* ========================================================================= *)
PROGRAM MPI;
  USES
    MP_Lex, MPI_SS;
  
  VAR
    filename: STRING;
    ok: BOOLEAN;
  
BEGIN (* MPI *)
  Write('MiniPascal source file > ');
  ReadLn(filename);

  InitScanner(filename, ok);
  IF (NOT ok) THEN BEGIN
    WriteLn('ERROR: Cannot open file ', filename);
    Halt;
  END; (* IF *)

  S;
  WriteLn;
END. (* MPI *)

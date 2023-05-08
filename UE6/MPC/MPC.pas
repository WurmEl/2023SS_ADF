(* MPC:                                                      SWa, 2023-05-03 *)
(* ----                                                                      *)
(* MiniPascal compiler main program.                                         *)
(* ========================================================================= *)
PROGRAM MPC;
  USES
    MP_Lex, MPC_SS, CodeDef, CodeGen, CodeInt, CodeDis;
  
  VAR
    filename: STRING;
    ok: BOOLEAN;
    ca: CodeArray;
  
BEGIN (* MPC *)
  Write('MiniPascal source file > ');
  ReadLn(filename);

  InitScanner(filename, ok);
  IF (NOT ok) THEN BEGIN
    WriteLn('ERROR: Cannot open file ', filename);
    Halt;
  END; (* IF *)

  S;
  IF (success) THEN BEGIN
    GetCode(ca);
    WriteLn;

    InterpretCode(ca);
    WriteLn;
    DisassembleCode(ca);
    WriteLn;
  END; (* IF *)
END. (* MPC *)

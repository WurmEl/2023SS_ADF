(* MPC:                                               Elias Wurm, 2023-05-03 *)
(* ------                                                                    *)
(* mini pascal compiler                                                      *)
(* ========================================================================= *)

program MPC;
uses
  MPLEX, MPC_SS, CodeDef, CodeGen, CodeInt, CodeDis;
  
var
  filename: STRING;
  ok: BOOLEAN;
  
begin (* MPI *)
  Write('MiniPascal source file > ');
  ReadLn(filename);

  InitScanner(filename, ok);
  if (not ok) then
  begin
    WriteLn('ERROR: Cannot open file ', filename);
    Halt;
  end; (* IF *)

  S;
  if (success) then
  begin
    GetCode(ca);
    WriteLn;

    InterpretCode(ca);
    WriteLn;
    DisassembleCode(ca);
    WriteLn;
  end;
end.

(* MPC:                                               Elias Wurm, 2023-05-03 *)
(* ------                                                                    *)
(* mini pascal compiler                                                      *)
(* ========================================================================= *)

program MPC;
uses
  MPLEX, MPC_SS, CodeDef;
  
var
  filename: STRING;
  ok: BOOLEAN;
  
begin (* MPI *)
  Write('enter filename > ');
  ReadLn(filename);
  InitScanner(filename, ok);
  if (not ok) then
  begin
    writeln('ERROR: file not found - ', filename);   
    HALT;
  end; (* IF *)

  S;
  if success then StoreCode(filename + 'o', ca) else writeln('Syntax error');
end.

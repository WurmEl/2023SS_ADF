(* MPI:                                               Elias Wurm, 2023-04-26 *)
(* ------                                                                    *)
(* mini pascal interpreter                                                   *)
(* ========================================================================= *)

program MPI;
uses
  MPLEX, MPP_S;
  
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
  if success then writeln('success') else writeln('Syntax error');
end.

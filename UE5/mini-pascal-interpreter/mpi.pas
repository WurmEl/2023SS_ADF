(* MPI:                                               Elias Wurm, 2023-04-26 *)
(* ------                                                                    *)
(* mini pascal interpreter                                                   *)
(* ========================================================================= *)

program MPI;
uses
  MPLEX;
  
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
  while (sy <> eofSy) do
  begin
    Write(sy, ' ');
    if (sy = numberSy) then WRITE('(', numberVal, ') ') else if(sy = identSy) then Write('(', identStr, ') '); (* IF *)
    NewSy;
  end; (* WHILE *)
end.

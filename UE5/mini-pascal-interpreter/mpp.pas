(* Mini Pascal Parser:                                Elias Wurm, 2023-04-26 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
unit MPP;

interface

var
  success: boolean;

implementation

uses MPLEX;

function SyIsNot(expectedSy: Symbol): boolean;
begin
  succes := success and (sy = expectedSy);
  SyIsNot := not succes;
end;

procedure S; forward;
procedure MP; forward;
procedure VarDecl; forward;
procedure StatSeq; forward;
procedure Stat; forward;
procedure Expr; forward;
procedure Term; forward;
procedure Fact; forward;

procedure S;
begin
  writeln('Parsing started ...');
  success := true;
  MP;
  if(not success or SyIsNot(eofSy)) then
    writeln('ERROR: System error in line ', syLnr, ' and column ', syLnr)
  else
    writeln('parse was successful');
end;

procedure MP;
begin
  SyIsNot(programSy);
  NewSy;
  VarDecl; if not success then exit;
  NewSy;
  SyIsNot(beginSy);
  StatSeq; if not success then exit;
  SyIsNot(endSy);
end;

end.

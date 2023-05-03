(* Mini Pascal Parser:                                Elias Wurm, 2023-04-26 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
unit MPP_S;

interface

procedure S;

var
  success: boolean;

implementation

uses MPLEX;

function SyIsNot(expectedSy: Symbol): boolean;
begin
  success := success and (sy = expectedSy);
  SyIsNot := not success;
end;

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
    writeln('ERROR: System error in line ', syLnr, ' and column ', syLnr, ' error with sy:', sy)
  else
    writeln('file interpreted successfully');
end;

procedure MP;
begin
  if SyIsNot(programSy) then exit;
  NewSy;
  if SyIsNot(identSy) then exit;
  NewSy;
  if SyIsNot(semicolonSy) then exit;
  NewSy;
  if(sy = varSy) then
  begin
    VarDecl; if not success then exit;
  end;
  if SyIsNot(beginSy) then exit;
  NewSy;
  StatSeq; if not success then exit;
  if SyIsNot(endSy) then exit;
  NewSy;
  if SyIsNot(periodSy) then exit;
  NewSy;
end;

procedure VarDecl;
begin
  if SyIsNot(varSy) then exit;
  NewSy;
  if SyIsNot(identSy) then exit;
  NewSy;
  while (sy = commaSy) do
  begin
    NewSy;
    if SyIsNot(identSy) then exit;
    NewSy;
  end;
  if SyIsNot(colonSy) then exit;
  NewSy;
  if SyIsNot(integerSy) then exit;
  NewSy;
  if SyIsNot(semicolonSy) then exit;
  NewSy;
end;

procedure StatSeq;
begin
  Stat; if not success then exit;
  while(sy = semicolonSy) do
  begin
    NewSy;
    Stat; if not success then exit;
  end;
end;

procedure Stat;
begin
  case sy of
    identSy:
    begin 
      NewSy; 
      if SyIsNot(assignSy) then exit;
      NewSy;
      Expr; if not success then exit;
    end;
    readSy: 
    begin
      NewSy;
      if SyIsNot(leftParSy) then exit;
      NewSy;
      if SyIsNot(identSy) then exit;
      NewSy;
      if SyIsNot(rightParSy) then exit;
      NewSy;
    end;
    writeSy:
    begin
      NewSy;
      if SyIsNot(leftParSy) then exit;
      NewSy;
      Expr; if not success then exit;
      if SyIsNot(rightParSy) then exit;
      NewSy;
    end;
  end;
end;

procedure Expr;
begin
  Term; if not success then exit;
  while(sy = plusSy) or (sy = minusSy) do
    case sy of
      plusSy:
      begin
        NewSy;
        Term; if not success then exit;
      end;
      minusSy:
      begin 
        NewSy;
        Term; if not success then exit;
      end;
    end;
end;

procedure Term;
begin
  Fact; if not success then exit;
  while(sy = timesSy) or (sy = divSy) do
    case sy of
      timesSy:
      begin
        NewSy;
        Fact; if not success then exit;
      end;
      divSy:
      begin 
        NewSy;
        Fact; if not success then exit;
      end;
    end;
end;

procedure Fact;
begin
  case sy of
    identSy: NewSy;
    numberSy: NewSy;
    leftParSy:
    begin 
      NewSy;
      Expr; if not success then exit;
      if SyIsNot(rightParSy) then exit;
      NewSy;
    end;
  else
    success := false;
  end;
end;


end.

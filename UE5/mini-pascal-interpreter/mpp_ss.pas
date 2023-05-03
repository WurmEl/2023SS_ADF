(* Mini Pascal Parser:                                Elias Wurm, 2023-04-26 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
unit MPP_SS;

interface

procedure S;

var
  success: boolean;

implementation

uses MPLEX, SymTab;

function SyIsNot(expectedSy: Symbol): boolean;
begin
  success := success and (sy = expectedSy);
  SyIsNot := not success;
end;

procedure SemErr(msg: string);
begin
  writeln('ERROR: semantic error - ', msg);
  success := false;
end;

procedure MPI; forward;
procedure VarDecl; forward;
procedure StatSeq; forward;
procedure Stat; forward;
procedure Expr(var e: integer); forward;
procedure Term(var t: integer); forward;
procedure Fact(var f: integer); forward;

procedure S;
begin
  writeln('Parsing started ...');
  success := true;
  MPI;
  if(not success or SyIsNot(eofSy)) then
    writeln('ERROR: in line ', syLnr, ' and column ', syLnr)
  else writeln('file interpreted successfully');
end;

procedure MPI;
begin
  (*SEM*) InitSymbolTable; (*ENDSEM*)
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
var
  ok: boolean;
begin
  if SyIsNot(varSy) then exit;
  NewSy;
  if SyIsNot(identSy) then exit;
  (*sem*) DeclVar(identStr, ok); (*endsem*)
  NewSy;
  while (sy = commaSy) do
  begin
    NewSy;
    if SyIsNot(identSy) then exit;
    (*sem*) DeclVar(identStr, ok); if not ok then SemErr('mult. decl.');(*endsem*)
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
var
  e: integer;
  destId: string;
begin
  case sy of
    identSy:
    begin
      (*sem*) destId := identStr; if not IsDecl(destId) then SemErr('var. not decl.'); (*endsem*)
      NewSy; 
      if SyIsNot(assignSy) then exit;
      NewSy;
      Expr(e); if not success then exit;
      (*sem*) if IsDecl(destId) then SetVal(destId, e); (*endsem*)
    end;
    readSy: 
    begin
      NewSy;
      if SyIsNot(leftParSy) then exit;
      NewSy;
      if SyIsNot(identSy) then exit;
      (*sem*) 
      if not IsDecl(identStr) then 
        SemErr('var. not decl.')
      else begin 
        write(identStr, ' > '); 
        ReadLn(e); 
        SetVal(identStr, e);
      end;
      (*endsem*)
      NewSy;
      if SyIsNot(rightParSy) then exit;
      NewSy;
    end;
    writeSy:
    begin
      NewSy;
      if SyIsNot(leftParSy) then exit;
      NewSy;
      Expr(e); if not success then exit;
      (*sem*) writeln(e); (*endsem*)
      if SyIsNot(rightParSy) then exit;
      NewSy;
    end;
  end;
end;

procedure Expr(var e: integer);
var
  t: integer;
begin
  Term(e); if not success then exit;
  while(sy = plusSy) or (sy = minusSy) do
    case sy of
      plusSy:
      begin
        NewSy;
        Term(t); if not success then exit;
        (* sem *) e := e + t; (* end sem *)
      end;
      minusSy:
      begin 
        NewSy;
        Term(t); if not success then exit;
        (* sem *) e := e - t; (* end sem *)
      end;
    end;
end;

procedure Term(var t: integer);
var
  f: integer;
begin
  Fact(t); if not success then exit;
  while(sy = timesSy) or (sy = divSy) do
    case sy of
      timesSy:
      begin
        NewSy;
        Fact(f); if not success then exit;
        (* sem *) t := t * f; (* end sem *)
      end;
      divSy:
      begin 
        NewSy;
        Fact(f); if not success then exit;
        (* sem *)if f = 0 then SemErr('zero division') else t := t div f; (* end sem *)
      end;
    end;
end;

procedure Fact(var f: integer);
begin
  case sy of
    identSy:
    begin
      (*sem*) if not IsDecl(identStr) then SemErr('var. not decl.') else GetVal(identStr, f); (*endsem*)
      NewSy;
    end;
    numberSy: 
    begin
      f := numberVal;
      NewSy;
    end;
    leftParSy:
    begin 
      NewSy;
      Expr(f); if not success then exit;
      if SyIsNot(rightParSy) then exit;
      NewSy;
    end;
  else
    success := false;
  end;
end;


end.

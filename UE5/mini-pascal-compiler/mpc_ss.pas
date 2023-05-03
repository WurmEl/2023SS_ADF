(* Mini Pascal Compiler:                              Elias Wurm, 2023-04-26 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
unit MPC_SS;

interface

uses CodeDef;

procedure S;

var
  success: boolean;
  ca: CodeArray;

implementation

uses MPLEX, SymTab, CodeGen;

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
procedure Expr; forward;
procedure Term; forward;
procedure Fact; forward;

procedure S;
begin
  writeln('Parsing started ...');
  success := true;
  MPI;
  if(not success or SyIsNot(eofSy)) then
    writeln('ERROR: in line ', syLnr, ' and column ', syLnr)
  else begin writeln('file compiled successfully'); GetCode(ca); end;
end;

procedure MPI;
begin
  (*SEM*) InitSymbolTable; InitCodeGenerator; (*ENDSEM*)
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
  (*sem*)Emit1(EndOpc);(*endsem*)
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
  destId: string;
begin
  case sy of
    identSy:
    begin
      (*sem*) destId := identStr; if not IsDecl(destId) then SemErr('var. not decl.') else Emit2(LoadAddrOpc, AddrOf(destId )); (*endsem*)
      NewSy; 
      if SyIsNot(assignSy) then exit;
      NewSy;
      Expr; if not success then exit;
      (*sem*) if IsDecl(destId) then Emit1(StoreOpc); (*endsem*)
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
      else
        Emit2(ReadOpc, AddrOf(identStr));
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
      Expr; if not success then exit;
      (*sem*) Emit1(WriteOpc); (*endsem*)
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
        (* sem *) Emit1(SubOpc);(* end sem *)
      end;
      minusSy:
      begin 
        NewSy;
        Term; if not success then exit;
        (* sem *) Emit1(AddOpc); (* end sem *)
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
        (* sem *) Emit1(MulOpc);(* end sem *)
      end;
      divSy:
      begin 
        NewSy;
        Fact; if not success then exit;
        (* sem *) Emit1(DivOpc); (* end sem *)
      end;
    end;
end;

procedure Fact;
begin
  case sy of
    identSy:
    begin
      (*sem*)  
      if not IsDecl(identStr) then
        SemErr('var. not decl.')
      else
        Emit2(LoadValOpc, AddrOf(identStr)); 
      (*endsem*)
      NewSy;
    end;
    numberSy: 
    begin
      (*sem*) Emit2(LoadConstOpc, numberVal); (*endsem*)
      NewSy;
    end;
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

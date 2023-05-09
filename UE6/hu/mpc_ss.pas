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

function NewNode: NodePtr;
var
  n: NodePtr;
begin
  New(n);
  n^.left := nil;
  n^.right := nil;
  n^.isOperator := false;
  n^.isIdent := false;
  NewNode := n;
end;

function CreateOperatorNode(left, right: NodePtr; opVal: string): NodePtr;
var
  n: NodePtr;
begin 
  n := NewNode; 
  n^.val := opVal; 
  n^.isOperator := true; 
  n^.left := left;
  n^.right := right;
  CreateOperatorNode := n;
end;

procedure MP; forward;
procedure VarDecl; forward;
procedure StatSeq; forward;
procedure Stat; forward;
procedure Expr(var e: NodePtr); forward;
procedure Term(var t: NodePtr); forward;
procedure Fact(var f: NodePtr); forward;

procedure S;
begin
  writeln('Parsing started ...');
  success := true;
  MP;
  if(not success or SyIsNot(eofSy)) then
    writeln('ERROR: in line ', syLnr, ' and column ', syLnr)
  else writeln('file compiled successfully');
end;

procedure MP;
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
  addr, addr1, addr2: integer;
  exprTree: ExprTreePtr;
begin
  case sy of
    identSy:
    begin
      (*sem*)
      destId := identStr;
      if (not IsDecl(destId)) then SemErr('variable not declared') else Emit2(LoadAddrOpc, AddrOf(destId));
      (*endsem*)
      NewSy;
      if (SyIsNot(assignSy)) then Exit;
      NewSy;
      (*sem*)exprTree := NewNode;(*endsem*)
      Expr(exprTree); if (not success) then Exit;
      (*sem*)
      EmitCodeForExprTree(exprTree);
      if (IsDecl(destId)) then Emit1(StoreOpc);
      (*endsem*)
    end;
    readSy:
    begin
      NewSy;
      if (SyIsNot(leftParSy)) then Exit;
      NewSy;
      if (SyIsNot(identSy)) then Exit;
      (*sem*)
      if (not IsDecl(identStr)) then SemErr('variable not declared') else Emit2(ReadOpc, AddrOf(identStr));
      (*endsem*)
      NewSy;
      if (SyIsNot(rightParSy)) then Exit;
      NewSy;
    end;
    writeSy:
    begin
      NewSy;
      if (SyIsNot(leftParSy)) then Exit;
      NewSy;
      (*sem*)exprTree := NewNode;(*endsem*)
      Expr(exprTree); if (not success) then Exit;
      (*sem*) 
      
      EmitCodeForExprTree(exprTree);
      Emit1(WriteOpc);
      (*endsem*)
      if (SyIsNot(rightParSy)) then Exit;
      NewSy;
    end;
    beginSy:
    begin
      NewSy;
      StatSeq; if not success then exit;
      if SyIsNot(endSy) then exit;
      NewSy;
    end;
    ifSy:
    begin
      NewSy;
      if SyIsNot(identSy) then exit;
      (*sem*)
      if not IsDecl(identStr) then SemErr('variable not declared');
      Emit2(LoadValOpc, AddrOf(identStr));
      Emit2(JmpZOpc, 0); (*0 as dummy address*)
      addr := CurAddr - 2;
      (*endsem*)
      NewSy;
      if SyIsNot(thenSy) then exit;
      NewSy;
      Stat; if not success then exit;
      while (sy = elseSy) do
      begin
        (*sem*) 
        Emit2(JmpOpc, 0); (*0 as dummy address*)
        FixUp(addr, CurAddr);
        addr := CurAddr - 2; 
        (*endsem*)
        NewSy;
        Stat; if not success then exit;
      end;
      (*sem*) FixUp(addr, CurAddr); (*endsem*)
    end;
    whileSy:
    begin
      NewSy;
      if SyIsNot(identSy) then exit;
      (*sem*)
      if not IsDecl(identStr) then SemErr('variable not declared');
      addr1 := CurAddr;
      Emit2(LoadValOpc, AddrOf(identStr));
      Emit2(JmpZOpc, 0); (*0 as dummy address*)
      addr2 := CurAddr - 2;
      (*endsem*)
      NewSy;
      if SyIsNot(doSy) then exit;
      NewSy;
      Stat; if not success then exit;
      (*sem*) Emit2(JmpOpc, addr1); FixUp(addr2, CurAddr); (*endsem*)
    end;
  end;
end;

procedure Expr(var e: NodePtr);
var
  right: NodePtr;
begin
  Term(e); if not success then exit;
  while(sy = plusSy) or (sy = minusSy) do
    case sy of
      plusSy:
      begin
        NewSy;
        (*sem*) right := NewNode; (*endsem*)
        Term(right); if not success then exit;
        (*sem*) e := CreateOperatorNode(e, right, '+'); (*endsem*)
      end;
      minusSy:
      begin 
        NewSy;
        (*sem*) right := NewNode; (*endsem*)
        Term(right); if not success then exit;
        (*sem*) e := CreateOperatorNode(e, right, '-'); (*endsem*)
      end;
    end;
end;

procedure Term(var t: NodePtr);
var
  right: NodePtr;
begin
  Fact(t); if not success then exit;
  while(sy = timesSy) or (sy = divSy) do
    case sy of
      timesSy:
      begin
        NewSy;
        (*sem*) right := NewNode; (*endsem*)
        Fact(right); if not success then exit;
        (*sem*) t := CreateOperatorNode(t, right, '*'); (*endsem*)
      end;
      divSy:
      begin 
        NewSy;
        (*sem*) right := NewNode; (*endsem*)
        Fact(right); if not success then exit;
        (*sem*) t := CreateOperatorNode(t, right, '/'); (*endsem*)
      end;
    end;
end;

procedure Fact(var f: NodePtr);
begin
  case sy of
    identSy:
    begin
      (*sem*) f^.val := identStr; f^.isIdent := true; f^.valInt := AddrOf(identStr); (*endsem*)
      NewSy;
    end;
    numberSy: 
    begin
      (*sem*) f^.val := 'const'; f^.valInt := numberVal; (*endsem*)
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

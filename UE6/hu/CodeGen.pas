(* CodeGen:                                         HDO, 2004-02-06
   -------
   Byte code generator for the MiniPascal compiler.
===================================================================*)
unit CodeGen;

(*$I Chooser.inc*)

interface

uses
  CodeDef;

procedure InitCodeGenerator;

procedure Emit1(opc: OpCode);
procedure Emit2(opc: OpCode; opd: INTEGER);

(*$IFDEF Midi*)
function  CurAddr: INTEGER;
procedure FixUp(addr: INTEGER; opd: INTEGER);
(*$ENDIF*)

procedure EmitCodeForExprTree(t: ExprTreePtr);

procedure GetCode(var ca: CodeArray);


implementation

var
  ca: CodeArray; (*array of opCodes and opderands*)
  n: INTEGER;    (*index of next free byte in c*)


procedure InitCodeGenerator;
(*-----------------------------------------------------------------*)
var
  i: INTEGER;
begin
  n := 1;
  for i := 1 to maxCodeLen do ca[i] := 0; (*FOR*)
end; (*InitCodeGenerator*)


procedure EmitByte(b: BYTE);
begin
  if n = maxCodeLen then
  begin
    WriteLn('*** Error: overflow in code array');
    HALT;
  end; (*IF*)
  ca[n] := b;
  n := n + 1;
end; (*EmitByte*)

procedure EmitWord(w: INTEGER);
begin
  EmitByte(w div 256);
  EmitByte(w mod 256);
end; (*EmitWord*)


procedure Emit1(opc: OpCode);
(*-----------------------------------------------------------------*)
begin
  EmitByte(Ord(opc));
end; (*Emit1*)

procedure Emit2(opc: OpCode; opd: INTEGER);
(*-----------------------------------------------------------------*)
begin
  EmitByte(Ord(opc));
  EmitWord(opd);
end; (*Emit1*)


(*$IFDEF Midi*)
function CurADdr: INTEGER;
  (*-----------------------------------------------------------------*)
begin
  CurAddr := n;
end; (*CurAddr*)

procedure FixUp(addr: INTEGER; opd: INTEGER);
(*-----------------------------------------------------------------*)
begin
  ca[addr    ] := opd div 256;
  ca[addr + 1] := opd mod 256;
end; (*FixUp*)
(*$ENDIF*)

// for testing purposes
procedure WriteTree(t: ExprTreePtr; indent: Integer);
var
  i: Integer;
begin
  if t <> nil then
  begin
    for i := 1 to indent do
      Write('  ');
    if not t^.isOperator and not t^.isIdent then 
      writeln(t^.val, ': ', t^.valInt) else WriteLn(t^.val);
    
    WriteTree(t^.left, indent + 1); 
    WriteTree(t^.right, indent + 1);
  end;
end;

procedure DisposeExprTree(t: ExprTreePtr);
begin
  if t <> nil then
  begin
    DisposeExprTree(t^.left);
    DisposeExprTree(t^.right);
    Dispose(t);
  end;
end;

procedure OptimizeExprTree(var t: ExprTreePtr);
var
  dummy: NodePtr;
begin
  if(t = nil) then Exit;

  OptimizeExprTree(t^.left);
  OptimizeExprTree(t^.right);

  // if left and right node are constant values remove the nodes  
  // and make the current node the result with the operation
  if t^.isOperator and (t^.left^.val = 'const') 
    and (t^.right^.val = 'const') then
  begin
    if (t^.val = '+') then 
      t^.left^.valInt := t^.left^.valInt + t^.right^.valInt
    else if (t^.val = '-') then 
      t^.left^.valInt := t^.left^.valInt - t^.right^.valInt
    else if (t^.val = '*') then 
      t^.left^.valInt := t^.left^.valInt * t^.right^.valInt
    else if (t^.val = '/') then 
      t^.left^.valInt := t^.left^.valInt div t^.right^.valInt;
    dummy := t^.left;
    Dispose(t^.right);
    Dispose(t);
    t := dummy;
  end
  // try to optimize add and sub expressions
  else if(t^.val = '+') or (t^.val = '-') then
  begin
    if not t^.left^.isOperator and not t^.left^.isIdent 
      and (t^.left^.valInt = 0) then
    begin
      if (t^.val = '-') then 
        // if our expr is 0 - a we want to invert a so we get -a 
        t^.right^.valInt := t^.right^.valInt * -1; 
      t := t^.right;
    end else 
    if not t^.right^.isOperator and not t^.right^.isIdent 
      and (t^.right^.valInt = 0) then 
      t := t^.left;
  end 
  // try to optimize mul and div expressions
  else if(t^.val = '*') or (t^.val = '/') then
    if (t^.val = '*') and not t^.left^.isOperator 
      and not t^.left^.isIdent and (t^.left^.valInt = 1) then 
      t := t^.right 
    else if not t^.right^.isOperator and not t^.right^.isIdent then
      if (t^.val = '/') and (t^.right^.valInt = 0) then
      begin 
        WriteLn('*** Error: div. by zero'); 
        HALT; 
      end else 
      if (t^.right^.valInt = 1) then t := t^.left;
end;

procedure RecursiveEmit(t: ExprTreePtr);
begin
  if(t = nil) then Exit;

  RecursiveEmit(t^.left);
  RecursiveEmit(t^.right);

  if t^.isOperator then
  begin
    // emit operator operation
    if (t^.val = '+') then Emit1(AddOpc)
    else if (t^.val = '-') then Emit1(SubOpc)
    else if (t^.val = '*') then Emit1(MulOpc)
    else if (t^.val = '/') then Emit1(DivOpc);
  end 
  else if t^.isIdent then
    // emit ident value
    Emit2(LoadValOpc, t^.valInt) 
  else
    // emit const value
    Emit2(LoadConstOpc, t^.valInt);
end;

procedure EmitCodeForExprTree(t: ExprTreePtr);
begin
  OptimizeExprTree(t);
  RecursiveEmit(t);
  
  // for testing purposes
  // WriteTree(t, 1); writeln;

  DisposeExprTree(t);
end;


procedure GetCode(var ca: CodeArray);
(*-----------------------------------------------------------------*)
begin
  ca := CodeGen.ca;
end; (*GetCode*)


end.
(*CodeGen*)
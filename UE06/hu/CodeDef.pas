(* CodeDef:                                         HDO, 2004-02-06
   -------
   Definition of the MiniPascal byte code.
===================================================================*)
unit CodeDef;

(*$I Chooser.inc*)

interface

const
  maxCodeLen = 100;

type
  OpCode = (      (*operands:*)
    LoadConstOpc, (*num  = numerical literal*)
    LoadValOpc,   (*addr = address of variable for value to load*)
    LoadAddrOpc,  (*addr = address of variable*)
    StoreOpc,
    AddOpc,
    SubOpc,
    MulOpc,
    DivOpc,
    ReadOpc,      (*addr = address of variable to read*)
    WriteOpc,
    (*$IFDEF Midi*)
    JmpOpc,       (*addr = code address to jump to*)
    JmpZOpc,      (*addr = code address to jump to on zero*)
    (*$ENDIF*)
    EndOpc);

  NodePtr = ^Node;
  Node = record
    left, right: NodePtr;
    val: string;
    valInt: integer;
    isOperator: boolean;
    isIdent: boolean;
  end;
  ExprTreePtr = NodePtr;

  CodeArray = array [1 .. maxCodeLen] of BYTE;


procedure StoreCode(fileName: STRING; ca: CodeArray);
procedure LoadCode(fileName: STRING; var ca: CodeArray; var ok: BOOLEAN);


implementation


procedure StoreCode(fileName: STRING; ca: CodeArray);
(*-----------------------------------------------------------------*)
var
  f: file of CodeArray;
begin
  Assign(f, fileName);
  ReWrite(f);
  Write(f, ca);
  Close(f);
end; (*StoreCode*)


procedure LoadCode(fileName: STRING; var ca: CodeArray; var ok: BOOLEAN);
(*-----------------------------------------------------------------*)
var
  f: file of CodeArray;
begin
  Assign(f, fileName);
  (*$I-*)
  Reset(f);
  (*$I+*)
  ok := IOResult = 0;
  if not ok then
    Exit;
  Read(f, ca);
  Close(f);
end; (*LoadCode*)


end.
(*CodeDef*)
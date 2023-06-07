(* Stack ADT Unit:                                    Elias Wurm, 2023-04-17 *)
(* ------                                                                    *)
(* A simple stack abstract data type.                                        *)
(* ========================================================================= *)
unit StackUnit;

interface

type
  Stack = Pointer;

function NewStack: Stack;
procedure DisposeStack(var s: Stack);
procedure Push(s: Stack; value: integer);
procedure Pop(s: Stack; var value: integer);
function IsEmpty(s: Stack): boolean;

implementation

type
  PNode = ^Node;
  Node = record
    data: integer;
    next: PNode;
  end;
  PList = PNode;
  PStack = ^StackRec;
  StackRec = record
    head: PList;
  end;

function NewStack: Stack;
var
  stack: PStack;
begin
  New(stack);
  stack^.head := nil;
  NewStack := stack;
end;

function NewNode(value: integer): PNode;
var
  n: PNode;
begin
  New(n);
  n^.data := value;
  n^.next := nil;
  NewNode := n;
end;

procedure Push(s: Stack; value: integer);
var
  n: PNode;
  ps: PStack;
begin
  ps := PStack(s);
  n := NewNode(value);
  n^.next := ps^.head;
  ps^.head := n;
  s := ps^.head;
end;

procedure Pop(s: Stack; var value: integer);
var
  n: PNode;
  ps: PStack;
begin
  ps := PStack(s);
  n := ps^.head;
  ps^.head := n^.next;
  s := ps^.head;
  value := n^.data;
  Dispose(n);
end;

function IsEmpty(s: Stack): boolean;
var
  ps: PStack;
begin
  ps := PStack(s);
  IsEmpty := ps^.head = nil;
end;

procedure DisposeStack(var s: Stack);
var
  ps: PStack;
  n: PNode;
begin
  ps := PStack(s);
  while(ps^.head <> nil) do
  begin
    n := ps^.head;
    ps^.head := n^.next;
    Dispose(n);
  end;
  Dispose(ps);
  s := nil;
end;

end.

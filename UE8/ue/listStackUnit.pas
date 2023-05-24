unit ListStackUnit;

interface

uses stackUnit;

type
  ListPtr = Pointer;

  ListStackPtr = ^ListStackObj;
  ListStackObj = object(StackObj)
  public
    constructor init;
    destructor done; virtual;

    procedure push(e: integer); virtual;
    procedure pop(var e: integer); virtual;
    function isEmpty: boolean; virtual;
  private
    data: ListPtr;
  end;

function NewListStack: ListStackPtr;

implementation

type
  NodePtr = ^Node;
  Node = record
    value: integer;
    next: NodePtr;
  end;

function newNode(value: integer): NodePtr;
var
  n: NodePtr;
begin
  New(n);
  n^.value := value;
  n^.next := nil;
  newNode := n;
end;

function NewListStack: ListStackPtr;
var
  stack: ListStackPtr;
begin
  New(stack, Init);
  NewListStack := stack;
end;

constructor ListStackObj.init;
begin
  data := nil;
  inherited init;
end;

destructor ListStackObj.done;
var
  n: NodePtr;
begin
  inherited done;
  while(data <> nil) do
  begin
    n := NodePtr(data);
    data := NodePtr(data)^.next;
    Dispose(n);
  end;
end;

procedure ListStackObj.push(e: integer);
var
  n: NodePtr;
begin
  n := newNode(e);
  n^.next := data;
  data := n;
end;


procedure ListStackObj.pop(var e: integer);
var
  n: NodePtr;
begin
  n := data;
  e := NodePtr(data)^.value;
  data := n^.next;
  Dispose(n);
end;

function ListStackObj.isEmpty: boolean;
begin
  isEmpty := data = nil;
end;

end.

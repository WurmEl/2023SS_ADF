(* List:                                              Elias Wurm, 2023-05-29 *)
(* ------                                                                    *)
(* A simple linked list implementation as class.                             *)
(* ========================================================================= *)
unit ListUnit;

interface

type
  ListPtr = ^ListNode;
  ListNode = object
    value: Integer;
    next: ListPtr;
  end;

  List = ^ListObj;
  ListObj = object
  public
    constructor Init;
    destructor Done; virtual;

    procedure Add(val: Integer);
    function Contains(val: Integer): Boolean;
    function Size: Integer;
    procedure Remove(val: Integer);
    procedure Clear;
  protected
    head: ListPtr;
    function NewNode(val: integer): ListPtr;
  end;

function NewList: List;

implementation

function NewList: List;
var
  l: List;
begin
  New(l, Init);
  NewList := l;
end;

constructor ListObj.Init;
begin
  head := nil;
end;

destructor ListObj.Done;
begin
  Clear;
end;

procedure ListObj.Add(val: Integer);
var
  node, lastNode: ListPtr;
begin
  node := NewNode(val);

  if head = nil then
    head := node
  else
  begin
    lastNode := head;
    while lastNode^.next <> nil do
      lastNode := lastNode^.next;
    lastNode^.next := node;
  end;
end;

function ListObj.Contains(val: Integer): Boolean;
var
  currentNode: ListPtr;
begin
  currentNode := head;
  while currentNode <> nil do
  begin
    if currentNode^.value = val then
    begin
      Contains := true;
      Exit;
    end;
    currentNode := currentNode^.next;
  end;
  Contains := false;
end;

function ListObj.Size: Integer;
var
  currentNode: ListPtr;
  i: integer;
begin
  currentNode := head;
  i := 0;
  while currentNode <> nil do
  begin
    Inc(i);
    currentNode := currentNode^.next;
  end;
  Size := i;
end;

procedure ListObj.Remove(val: Integer);
var
  currentNode, prevNode, tempNode: ListPtr;
begin
  prevNode := nil;
  currentNode := head;

  while currentNode <> nil do
    if currentNode^.value = val then
    begin
      if prevNode = nil then
      begin
        // The node to remove is the head node
        tempNode := head;
        head := currentNode^.next;
        Dispose(tempNode);
        currentNode := head;
      end
      else
      begin
        // The node to remove is not the head node
        tempNode := currentNode;
        prevNode^.next := currentNode^.next;
        currentNode := currentNode^.next;
        Dispose(tempNode);
      end;
    end
    else
    begin
      prevNode := currentNode;
      currentNode := currentNode^.next;
    end;
end;

procedure ListObj.Clear;
var
  currentNode, tempNode: ListPtr;
begin
  currentNode := head;
  while currentNode <> nil do
  begin
    tempNode := currentNode;
    currentNode := currentNode^.next;
    Dispose(tempNode);
  end;
  head := nil;
end;

function ListObj.NewNode(val: integer): ListPtr;
var
  node: ListPtr;
begin
  New(node);
  node^.value := val;
  node^.next := nil;
  NewNode := node;
end;

end.

unit SortedListUnit;

interface

uses
  ListUnit;

type
  SortedList = ^SortedListObj;
  SortedListObj = object(ListObj)
    constructor Init;
    destructor Done; virtual;

    procedure Add(val: Integer); virtual;
    function Contains(val: Integer): Boolean; virtual;
  end;

function NewSortedList: SortedList;

implementation

function NewSortedList: SortedList;
var 
  sl: SortedList;
begin
  New(sl, Init);
  NewSortedList := sl;
end;

constructor SortedListObj.Init;
begin
  inherited Init;
end;

destructor SortedListObj.Done;
begin
  inherited Done;
end;

procedure SortedListObj.Add(val: Integer);
var
  node, currentNode, prevNode: ListPtr;
begin
  node := NewNode(val);

  if head = nil then
    head := node
  else if val < head^.value then
  begin
    node^.next := head;
    head := node;
  end
  else
  begin
    prevNode := head;
    currentNode := head^.next;

    while (currentNode <> nil) and (currentNode^.value < val) do
    begin
      prevNode := currentNode;
      currentNode := currentNode^.next;
    end;

    node^.next := currentNode;
    prevNode^.next := node;
  end;
end;

function SortedListObj.Contains(val: Integer): Boolean;
var
  currentNode: ListPtr;
begin
  currentNode := head;

  while (currentNode <> nil) and (currentNode^.value < val) do
    currentNode := currentNode^.next;

  Contains := (currentNode <> nil) and (currentNode^.value = val);
end;

end.

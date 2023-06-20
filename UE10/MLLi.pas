unit MLLi;

interface

uses
  MLObj, MLColl;

type
  MLListNodePtr = ^MLListNode;
  MLListNode = record
    obj: MLObject;
    next: MLListNodePtr;
  end;

  (* === class MLList === *)

  MLList = ^MLListObj;
  MLListObj = object(MLCollectionObj)
    head: MLListNodePtr;
    
    constructor Init;
    destructor Done; virtual;
    
    function Size: INTEGER; virtual;
    procedure Add(o: MLObject); virtual;
    function Remove(o: MLObject): MLObject; virtual;
    function Contains(o: MLObject): BOOLEAN; virtual;
    procedure Clear; virtual;
    function NewIterator: MLIterator; virtual;
    
    procedure Prepend(o: MLObject);
  end;

  (* === class MLListIterator === *)

  MLListIterator = ^MLListIteratorObj;
  MLListIteratorObj = object(MLIteratorObj)
    curNode: MLListNodePtr;
    
    constructor Init(l: MLList);
    destructor Done; virtual;
    
    function Next: MLObject; virtual;
  end;

function NewMLList: MLList;

implementation

function NewMLList: MLList;
var
  l: MLList;
begin
  New(l, Init);
  NewMLList := l;
end;

(* === class MLList === *)

constructor MLListObj.Init;
begin
  inherited Init;
  Register('MLList', 'MLCollection');
  head := NIL;
end;

destructor MLListObj.Done;
begin
  Clear;
  inherited Done;
end;

function MLListObj.Size: INTEGER;
var
  count: INTEGER;
  curNode: MLListNodePtr;
begin
  count := 0;
  curNode := head;
  while curNode <> NIL do
  begin
    curNode := curNode^.next;
    Inc(count);
  end;
  Size := count;
end;

procedure MLListObj.Add(o: MLObject);
var
  newNode: MLListNodePtr;
  curNode: MLListNodePtr;
begin
  New(newNode);
  newNode^.obj := o;
  newNode^.next := NIL;

  if head = NIL then
    head := newNode
  else begin
    curNode := head;
    while curNode^.next <> NIL do
      curNode := curNode^.next;
    curNode^.next := newNode;
  end;
end;

function MLListObj.Remove(o: MLObject): MLObject;
var
  prevNode, curNode: MLListNodePtr;
begin
  if head = NIL then
  begin
    Remove := NIL;
    Exit;
  end;

  prevNode := NIL;
  curNode := head;
  while (curNode <> NIL) and (curNode^.obj <> o) do
  begin
    prevNode := curNode;
    curNode := curNode^.next;
  end;

  if curNode = NIL then
  begin
    Remove := NIL;
    Exit;
  end;

  if prevNode = NIL then
    head := curNode^.next
  else
    prevNode^.next := curNode^.next;

  Remove := curNode^.obj;
  Dispose(curNode);
end;

function MLListObj.Contains(o: MLObject): BOOLEAN;
var
  curNode: MLListNodePtr;
begin
  curNode := head;
  while curNode <> NIL do
  begin
    if curNode^.obj^.IsEqualTo(o) then
    begin
      Contains := TRUE;
      Exit;
    end;
    curNode := curNode^.next;
  end;
  Contains := FALSE;
end;

procedure MLListObj.Clear;
var
  curNode, nextNode: MLListNodePtr;
begin
  curNode := head;
  while curNode <> NIL do
  begin
    nextNode := curNode^.next;
    Dispose(curNode^.obj, Done);
    Dispose(curNode);
    curNode := nextNode;
  end;
  head := NIL;
end;

function MLListObj.NewIterator: MLIterator;
var
  iterator: MLListIterator;
begin
  New(iterator, Init(@Self));
  NewIterator := iterator;
end;

procedure MLListObj.Prepend(o: MLObject);
var
  newNode: MLListNodePtr;
begin
  New(newNode);
  newNode^.obj := o;
  newNode^.next := head;
  head := newNode;
end;

(* === class MLListIterator === *)

constructor MLListIteratorObj.Init(l: MLList);
begin
  inherited Init;
  curNode := l^.head;
end;

destructor MLListIteratorObj.Done;
begin
  inherited Done;
end;

function MLListIteratorObj.Next: MLObject;
begin
  if curNode <> NIL then
  begin
    Next := curNode^.obj;
    curNode := curNode^.next;
  end
  else
    Next := NIL;
end;

end.

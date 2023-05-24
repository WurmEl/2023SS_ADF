unit ArrayStackUnit;

interface

uses stackUnit;

type
  intArray = array[0..1] of integer;

  ArrayStackPtr = ^ArrayStackObj;
  ArrayStackObj = object(StackObj)
  public
    constructor init(size: integer);
    destructor done; virtual;

    procedure push(e: integer); virtual;
    procedure pop(var e: integer); virtual;
    function isEmpty: boolean; virtual;
    function isFull: boolean; virtual;
  protected
    size:integer;
    top: integer;
    data: ^intArray;
  end;

function NewArrayStack(size: integer): ArrayStackPtr;

implementation

function NewArrayStack(size: integer): ArrayStackPtr;
var
  stack: ArrayStackPtr;
begin
  New(stack, Init(size));
  NewArrayStack := stack;
end;

constructor ArrayStackObj.init(size: integer);
begin
  inherited init;
  self.size := size;
  self.top := 0;
  GetMem(self.data, self.size * sizeOf(integer));
end;

destructor ArrayStackObj.done;
begin
  FreeMem(self.data, self.size * sizeOf(integer));
  inherited done;
end;

procedure ArrayStackObj.push(e: integer);
begin
  Inc(top);
  {$r-}
  data^[top] := e;
  {$r+}
end;

procedure ArrayStackObj.pop(var e: integer);
begin
  {$r-}
  e := data^[top];
  {$r+}
  Dec(top);
end;

function ArrayStackObj.isEmpty: boolean;
begin
  isEmpty := top = 0;
end;

function ArrayStackObj.isFull: boolean;
begin
  isFull := top = size;
end;

end.

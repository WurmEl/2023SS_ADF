unit FlexibleArrayStackUnit;

interface

uses ArrayStackUnit;

type
  FlexibleArrayStackPtr = ^FlexibleArrayStackObj;
  FlexibleArrayStackObj = object(ArrayStackObj)
  public
    constructor init;
    destructor done; virtual;

    procedure push(e: integer); virtual;
    procedure pop(var e: integer); virtual;
    function isFull: boolean; virtual;

  private
    procedure resize;
  end;

function NewFlexibleArrayStack: FlexibleArrayStackPtr;

implementation

const
  initSize = 10;

function NewFlexibleArrayStack: FlexibleArrayStackPtr;
var
  stack: FlexibleArrayStackPtr;
begin
  New(stack, Init);
  NewFlexibleArrayStack := stack;
end;

constructor FlexibleArrayStackObj.init;
begin
  inherited init(initSize);
end;

destructor FlexibleArrayStackObj.done;
begin
  inherited done;
end;

procedure FlexibleArrayStackObj.push(e: integer);
begin
  if inherited isFull then resize;
  inherited push(e);
end;

procedure FlexibleArrayStackObj.pop(var e: integer);
begin
  if (isEmpty) then
  begin
    writeln('ERROR: stack empty');
    halt;
  end;
  inherited pop(e);
end;

procedure FlexibleArrayStackObj.resize;
var
  newData: ^intArray;
  i: integer;
begin
  GetMem(newData, size * 2 * sizeOf(integer));
  for i := 0 to top do {$r-}newData^[i] := data^[i]{$r+};
  FreeMem(data, size * sizeOf(integer));
  size := size * 2;
  data := newData;
end;

function FlexibleArrayStackObj.IsFull: boolean;
begin
  IsFull := false;
end;

end.

unit SaveArrayStackUnit;

interface

uses ArrayStackUnit;

type
  SaveArrayStackPtr = ^SaveArrayStackObj;
  SaveArrayStackObj = object(ArrayStackObj)
  public
    constructor init(size: integer);
    destructor done; virtual;

    procedure push(e: integer); virtual;
    procedure pop(var e: integer); virtual;
  end;

function NewSaveArrayStack(size: integer): SaveArrayStackPtr;

implementation

function NewSaveArrayStack(size: integer): SaveArrayStackPtr;
var
  stack: SaveArrayStackPtr;
begin
  New(stack, Init(size));
  NewSaveArrayStack := stack;
end;

constructor SaveArrayStackObj.init(size: integer);
begin
  if(size < 0) then
  begin 
    writeln('ERROR: size must be bigger than 0'); 
    halt; 
  end;

  inherited init(size);
end;

destructor SaveArrayStackObj.done;
begin
  inherited done;
end;

procedure SaveArrayStackObj.push(e: integer);
begin
  if (isFull) then
  begin
    writeln('ERROR: stack full');
    halt;
  end;
  inherited push(e);
end;

procedure SaveArrayStackObj.pop(var e: integer);
begin
  if (isEmpty) then
  begin
    writeln('ERROR: stack empty');
    halt;
  end;
  inherited pop(e);
end;

end.

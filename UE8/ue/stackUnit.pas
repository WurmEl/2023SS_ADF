unit StackUnit;

interface

type
  StackPtr = ^StackObj;
  StackObj = object

  public
    constructor init;
    destructor done; virtual;

    procedure push(e: integer); virtual; abstract;
    procedure pop(var e: integer); virtual; abstract;
    function isEmpty: boolean; virtual; abstract;
  end;

implementation

constructor StackObj.init;
begin
end;

destructor StackObj.done;
begin
end;

end.

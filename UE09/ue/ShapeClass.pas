unit ShapeClass;

interface

uses Windows;

type
  ShapePtr = ^ShapeObj;
  ShapeObj = object
  public
    constructor init(dc:HDC);
    destructor done; virtual;

    procedure draw; virtual; abstract;
    procedure moveBy(dx,dy:integer); virtual; abstract;

    function shallowClone: ShapePtr;
    function clone: ShapePtr; virtual;

  protected
    dc: HDC;
  end;

implementation

constructor ShapeObj.init(dc: HDC);
begin
  self.dc := dc;
end;

destructor ShapeObj.done;
begin
end;

function ShapeObj.shallowClone: ShapePtr;
var
  cl: ShapePtr;
begin
  GetMem(cl, SizeOf(self));
  Move(self, pointer(cl), SizeOf(self));
  shallowClone := cl;
end;

function ShapeObj.clone: ShapePtr;
begin
  clone := shallowClone;
end;

end.

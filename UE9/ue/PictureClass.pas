unit PictureClass;

interface

uses ShapeClass, Windows, WinGraph;

type
  PicturePtr = ^PictureObj;
  PictureObj = object(ShapeObj)
  public
    constructor init(dc: HDC);
    destructor done; virtual;

    procedure draw; virtual;
    procedure moveBy(dx,dy:integer); virtual;
    function clone: ShapePtr; virtual;

    procedure addShape(shape: ShapePtr);
  
  private
    shapes: array[0..10] of ShapePtr;
    count: integer;
  end;

implementation

constructor PictureObj.init(dc: HDC);
begin
  inherited init(dc);
  self.count := 0;
end;

destructor PictureObj.done;
var
  i: integer;
begin
  for i := 0 to count-1 do
    Dispose(shapes[i], done);
  inherited done;
end;

procedure PictureObj.draw;
var
  i: integer;
begin
  for i := 0 to count-1 do
    shapes[i]^.draw;
end;

procedure PictureObj.moveBy(dx,dy:integer);
var
  i: integer;
begin
  for i := 0 to count-1 do
    shapes[i]^.moveBy(dx,dy);
end;

procedure PictureObj.addShape(shape: ShapePtr);
begin
  self.shapes[self.count] := shape;
  self.count := self.count + 1;
end;

function PictureObj.clone: ShapePtr;
var
  c: PicturePtr;
  i: integer;
begin
  c := PicturePtr(inherited clone);
  for i := 0 to count-1 do
    c^.shapes[i] := shapes[i]^.clone;
  clone := ShapePtr(c);
end;

end.

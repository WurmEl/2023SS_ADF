unit CircleClass;

interface

uses ShapeClass, Windows, WinGraph;

type
  CirclePtr = ^CircleObj;
  CircleObj = object(ShapeObj)
  public
    constructor init(dc: HDC; x, y, rad: integer);
    destructor done; virtual;

    procedure draw; virtual;
    procedure moveBy(dx,dy:integer); virtual;

  private
    x, y, rad: integer;
  end;

function NewCircle(dc: HDC; x,y,r: integer): CirclePtr;

implementation

function NewCircle(dc: HDC; x,y,r: integer): CirclePtr;
var
  circle: CirclePtr;
begin
  New(circle, init(dc, x,y,r));
  circle^.draw;
  NewCircle := circle;
end;

constructor CircleObj.init(dc: HDC; x, y, rad: integer);
begin
  inherited init(dc);
  self.x := x;
  self.y := y;
  self.rad := rad;
end;

destructor CircleObj.done;
begin
  inherited done;
end;

procedure CircleObj.draw;
begin
  Ellipse(dc, x,y,x+rad*2,y+rad*2);
end;

procedure CircleObj.moveBy(dx,dy:integer);
begin
  x := x + dx;
  y := y + dy;
end;

end.

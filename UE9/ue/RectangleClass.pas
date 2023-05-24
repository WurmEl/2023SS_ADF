unit RectangleClass;

interface

uses ShapeClass, Windows, WinGraph;

type
  RectanglePtr = ^RectangleObj;
  RectangleObj = object(ShapeObj)
  public
    constructor init(dc: HDC; x1, y1, x2, y2: integer);
    destructor done; virtual;

    procedure draw; virtual;
    procedure moveBy(dx,dy:integer); virtual;

  private
    x1, y1, x2, y2: integer;
  end;

implementation

constructor RectangleObj.init(dc: HDC; x1, y1, x2, y2: integer);
begin
  inherited init(dc);
  self.x1 := x1;
  self.y1 := y1;
  self.x2 := x2;
  self.y2 := y2;
end;

destructor RectangleObj.done;
begin
  inherited done;
end;

procedure RectangleObj.draw;
begin
  Rectangle(dc, x1, y1, x2, y2);
end;

procedure RectangleObj.moveBy(dx,dy:integer);
begin
  x1 := x1 + dx;
  y1 := y1 + dy;
  x2 := x2 + dx;
  y2 := y2 + dy;
end;

end.

unit LineClass;

interface

uses ShapeClass, Windows, WinGraph;

type
  LinePtr = ^LineObj;
  LineObj = object(ShapeObj)
  public
    constructor init(dc: HDC; x1, y1, x2, y2: integer);
    destructor done; virtual;

    procedure draw; virtual;
    procedure moveBy(dx,dy:integer); virtual;

  private
    x1, y1, x2, y2: integer;
  end;

implementation

constructor LineObj.init(dc: HDC; x1, y1, x2, y2: integer);
begin
  inherited init(dc);
  self.x1 := x1;
  self.x2 := x2;
  self.y1 := y1;
  self.y2 := y2;
end;

destructor LineObj.done;
begin
  inherited done;
end;

procedure LineObj.draw;
begin
  MoveTo(dc, x1, x2);
  LineTo(dc, x2, y2);
end;

procedure LineObj.moveBy(dx,dy:integer);
begin
  x1 := x1 + dx;
  y1 := y1 + dy;
  x2 := x2 + dx;
  y2 := y2 + dy;
end;

end.

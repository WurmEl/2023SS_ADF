program TestProgram;

uses PictureClass, LineClass, RectangleClass, CircleClass, ShapeClass, Windows, WinCrt, WinGraph; 

procedure drawShape(dc: HDC; wnd: HWnd; r: TRect);
var
  picture: PicturePtr;
  shape: ShapePtr;
begin
  New(picture, init(dc));
  picture^.addShape(NewCircle(dc, 0, 0, 30));
  picture^.addShape(NewCircle(dc, 0, 0, 20));

  shape := picture^.clone;

  picture^.draw;
  shape^.draw;

  Dispose(picture, done);
  Dispose(shape, done);
end;

begin
  redrawProc := drawShape;
  WGMain;
end.

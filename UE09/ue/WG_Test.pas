(* WG_Test:                      HDO, 1995-09-11; GHO 2010-05-28, HDO 2011
   -------
   Test program for generating Windows graphics based on unit WinGraph.
==========================================================================*)
PROGRAM WG_Test;

  USES
  {$IFDEF FPC}
    Windows,
  {$ELSE}
    WinTypes, WinProcs,
  {$ENDIF}
    Strings, WinCrt,
    WinGraph;

  PROCEDURE Redraw_Example(dc: HDC; wnd: HWnd; r: TRect); FAR;
    VAR
      txt: ARRAY [0..255] OF CHAR;
      n, i: INTEGER;
      incX, incY: REAL;
  BEGIN
    StrCopy(txt, 'Some Text');
    TextOut(dc, 10, 10, txt, StrLen(txt));
    Ellipse(dc, 50, 50, 200, 100);
    n := 50;
    incX := (r.right  - r. left) / n;
    incY := (r.bottom - r.top)   / n;
    FOR i := 1 TO n DO BEGIN
      MoveTo (dc, Round(i*incX), 0);
      LineTo(dc, Round(incX * (n -i)), Round(i*incy));
    END; (*FOR*)
  END; (*Redraw_Example*)

  PROCEDURE Redraw_Example2(dc: HDC; wnd: HWnd; r: TRect); FAR;
  BEGIN
    (*... draw anything ...*)
  END; (*Redraw_Example2*)

  PROCEDURE MousePressed_Example(dc: HDC; wnd: HWnd; x, y: INTEGER); FAR;
  BEGIN
    WriteLn('mouse pressed at: ', x, ', ', y);
    (*InvalidateRect(wnd, NIL, TRUE);*)
  END; (*MousePressed_Exmple*)

BEGIN (*WG_Test*)
  redrawProc := Redraw_Example;
  mousePressedProc := MousePressed_Example;
  WGMain;
END. (*WG_Test*)

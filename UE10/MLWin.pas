(*MLWin:                                               MiniLib V.4, 2004
  -----
  Class MLWindow.
========================================================================*)
unit MLWin;

interface

uses
  OSBridge, MLObj;

type

  (*=== class MLWindow ===*)

  MLWindow    = ^MLWindowObj;
  MLWindowObj = object(MLObjectObj)

    (*--- private data components ---*)

    winTitle: STRING;
    wp:       WindowPeer;

    constructor Init(title: STRING);
    destructor Done; virtual;

    (*--- overridden method ---*)

    function IsEqualTo(o: MLObject): BOOLEAN; virtual;
    (*returns whether both MLWindow objects are identical*)

    (*--- new methods ... ---*)

    function GetWindowPeer: WindowPeer;

    (*--- ... for window handling ---*)

    procedure Open; virtual;
    procedure Close; virtual;
    procedure Update; virtual;

    procedure GetContentRect(var topLeft: Point; var w, h: INTEGER); virtual;
    function  Width:  INTEGER; virtual; (*returns same w as GetContent*)
    function  Height: INTEGER; virtual; (*returns same h as GetContent*)
    function  IsVisible: BOOLEAN; virtual;

    procedure Redraw; virtual;          (*override to do drawing*)

    procedure InvalRectangle(topLeft: Point; w, h: INTEGER); virtual;
    procedure InvalContent; virtual;    (*InvalRectangle for whole content*)
    procedure EraseContent; virtual;

    (*--- ... for event handling ---*)

    procedure OnIdle; virtual;
    procedure OnMousePressed (pos: Point); virtual;
    procedure OnMouseReleased(pos: Point); virtual;
    procedure OnMouseMove    (pos: Point); virtual;
    procedure OnKey(key: CHAR); virtual;
    procedure OnCommand(commandNr: INTEGER);
    procedure OnRedraw; virtual;
    procedure OnResize; virtual;

    procedure GetMouseState(var buttonPressed: BOOLEAN; var pos: Point); virtual;

    (*--- ... for drawing: w = width, h = height, t = thickness ---*)

    procedure DrawDot(pos: Point); virtual;
    procedure DrawLine(startPos, endPos: Point; t: INTEGER); virtual;
    procedure DrawRectangle(topLeft: Point; w, h, t: INTEGER); virtual;
    procedure DrawFilledRectangle(topLeft: Point; w, h: INTEGER); virtual;
    procedure DrawOval(topLeft: Point; w, h, t: INTEGER); virtual;
    procedure DrawFilledOval(topLeft: Point; w, h: INTEGER); virtual;
    procedure DrawString(pos: Point; str: STRING; size: INTEGER);

  end; (*MLWindow*)

function NewMLWindow(title: STRING): MLWindow;


(*======================================================================*)
implementation

uses
  MetaInfo,
  MLAppl;


function NewMLWindow(title: STRING): MLWindow;
var
  w: MLWindow;
begin
  New(w, Init(title));
  NewMLWindow := w;
end; (*NewMLWindow*)


(*=== MLWindow ===*)
 
 
constructor MLWindowObj.Init(title: STRING);
begin
  inherited Init;
  Register('MLWindow', 'MLObject');
  winTitle := title;
  wp       := NIL;
end; (*MLWindowObj.Init*)

destructor MLWindowObj.Done;
begin
  Close;
  inherited Done;
end; (*MLWindowObj.Done*)


(*--- overridden method ---*)

function MLWindowObj.IsEqualTo(o: MLObject): BOOLEAN;
begin
  IsEqualTo := (@SELF = o);
end; (*MLWindowObj.IsEqualTo*)


(*--- new methods ... ---*)

function MLWindowObj.GetWindowPeer: WindowPeer;
begin
  GetWindowPeer := wp;
end; (*MLWindowObj.GetWindowPeer*)


(*--- ... for window handling ---*)

procedure MLWindowObj.Open;
begin
  OSB_CreateNewWindow(winTitle, wp);
  MLApplicationInstance^.AddWindow(@SELF);
end; (*MLWindowObj.Open*)
    
procedure MLWindowObj.Close;
begin
  MLApplicationInstance^.RemoveWindow(@SELF);
  OSB_DestroyOldWindow(wp);
end; (*MLWindowObj.Close*)

procedure MLWindowObj.Update;
begin
  if IsVisible then
  begin
    OSB_EraseContent(wp);
    Redraw;
  end; (*IF*)
end; (*MLWindowObj.Update*)


procedure MLWindowObj.GetContentRect(var topLeft: Point;
  var w, h : INTEGER);
begin
  OSB_GetContentRect(wp, topLeft, w, h);
end; (*MLWindowObj.GetContentRect*)
    
function MLWindowObj.Width: INTEGER;
var
  topLeft: Point;
  w, h:    INTEGER;
begin
  OSB_GetContentRect(wp, topLeft, w, h);
  Width := w;
end; (*MLWindowObj.Width*)
    
function MLWindowObj.Height: INTEGER;
var
  topLeft: Point;
  w, h:    INTEGER;
begin
  OSB_GetContentRect(wp, topLeft, w, h);
  Height := h;
end; (*MLWindowObj.Height*)
    
function MLWindowObj.IsVisible: BOOLEAN;
begin
  IsVisible := OSB_IsVisible(wp);
end; (*MLWindowObj.IsVisible*)


procedure MLWindowObj.Redraw;
begin
  (*nothing to do here: override to do drawing*)
end; (*MLWindowObj.Redraw*)


procedure MLWindowObj.InvalRectangle(topLeft: Point; w, h: INTEGER);
begin
  OSB_InvalRect(wp, topLeft, w, h);
end; (*MLWindowObj.InvalRectangle*)

procedure MLWindowObj.InvalContent;
var
  topLeft: Point;
  w, h:    INTEGER;
begin
  GetContentRect(topLeft, w, h);
  InvalRectangle(topLeft, w, h);
end; (*MLWindowObj.InvalContent*)

procedure MLWindowObj.EraseContent;
begin
  OSB_EraseContent(wp);
end; (*MLWindowObj.EraseContent*)


(*--- ... for event handling ---*)

procedure MLWindowObj.OnIdle;
begin
  (*nothing to do here*)
end; (*MLWindowObj.OnIdle*)
    
procedure MLWindowObj.OnMousePressed(pos: Point);
begin
  (*nothing to do here*)
end; (*MLWindowObj.OnMousePressed*)
    
procedure MLWindowObj.OnMouseReleased(pos: Point);
begin
  (*nothing to do here*)
end; (*MLWindowObj.OnMOuseReleased*)
    
procedure MLWindowObj.OnMouseMove(pos: Point);
begin
  (*nothing to do here*)
end; (*MLWindowObj.OnMouseMove*)
    
procedure MLWindowObj.OnKey(key: CHAR);
begin
  (*nothing to do here*)
end; (*MLWindowObj.OnMouseMove*)
    
procedure MLWindowObj.OnCommand(commandNr: INTEGER);
var
  w: MLWindow;
begin
  if commandNr = closeCommand then
  begin
    Close;
    w := @SELF;
    Dispose(w, Done);
  end; (*IF*)
end; (*MLWindowObj.OnCommand*)
    
procedure MLWindowObj.OnRedraw;
begin
  if IsVisible then
    Redraw;
end; (*MLWindowObj.OnRedraw*)

procedure MLWindowObj.OnResize;
begin
  if IsVisible then
    Update;
end; (*MLWindowObj.OnResize*)


procedure MLWindowObj.GetMouseState(var buttonPressed: BOOLEAN;
  var pos: Point);
begin
  OSB_GetMouseState(wp, buttonPressed, pos);
end; (*MLWindowObj.GetMouseState*)


(*--- ... for drawing ---*)

procedure MLWindowObj.DrawDot(pos: Point);
begin
  if IsVisible then
    OSB_DrawDot(wp, pos);
end; (*MLWindowObj.DrawDot*)

procedure MLWindowObj.DrawLine(startPos, endPos: Point; t: INTEGER);
begin
  if IsVisible then
    OSB_DrawLine(wp, startPos, endPos, t);
end; (*MLWindowObj.DrawLine*)

procedure MLWindowObj.DrawRectangle(topLeft: Point; w, h, t: INTEGER);
begin
  if IsVisible then
    OSB_DrawRectangle(wp, topLeft, w, h, t, FALSE);
end; (*MLWindowObj.DrawRectangle*)

procedure MLWindowObj.DrawFilledRectangle(topLeft: Point; w, h: INTEGER);
begin
  if IsVisible then
    OSB_DrawRectangle(wp, topLeft, w, h, 1, TRUE);
end; (*MLWindowObj.DrawFilledRectangle*)

procedure MLWindowObj.DrawOval(topLeft: Point; w, h, t: INTEGER);
begin
  if IsVisible then
    OSB_DrawOval(wp, topLeft, w, h, t, FALSE);
end; (*MLWindowObj.DrawOval*)

procedure MLWindowObj.DrawFilledOval(topLeft: Point; w, h: INTEGER);
begin
  if IsVisible then
    OSB_DrawOval(wp, topLeft, w, h, 1, TRUE);
end; (*MLWindowObj.DrawFilledOval*)

procedure MLWindowObj.DrawString(pos: Point; str: STRING; size: INTEGER);
begin
  if IsVisible then
    OSB_DrawString(wp, pos, str, size);
end; (*MLWindowObj.DrawString*)


end. (*MLWin*)
 
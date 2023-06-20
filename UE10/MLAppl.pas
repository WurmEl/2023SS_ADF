(*MLAppl:                                              MiniLib V.4, 2004
  ------
  Class MLApplication.
========================================================================*)
unit MLAppl;

interface
  
uses
  OSBridge,
  MLObj, MLColl, MLVect,
  MLWin;
    
type

  (*=== class MLApplication ===*)

  MLApplication   = ^MLApplicationObj;
  MLApplicationObj = object(MLObjectObj)

    (*--- private data components ---*)

    applName:    STRING;   (*application name*)
    running:     BOOLEAN;  (*TRUE while running*)
    openWindows: MLVector; (*currently open windows*)

    constructor Init(name: STRING);
    destructor Done; virtual;

    (*--- new private methods, no overrides ---*)

    procedure AddWindow   (w: MLWindow); (*called from MLWindow.Open*)
    procedure RemoveWindow(w: MLWindow); (*called from MLwindow.Close*)
    procedure DispatchEvent(e: Event);

    (*--- override to build application specific menus ---*)

    procedure BuildMenus; virtual; (*using calls to NewMenuCommand*)

    (*--- rund and quit methods ---*)

    procedure Run; virtual;
    procedure Quit; virtual;

    (*--- window handling methods ---*)

    procedure OpenNewWindow; virtual;
    function  WindowOf(wp: WindowPeer): MLWindow; virtual;
    function  TopWindow: MLWindow; virtual;
    function  NewOpenWindowsIterator: MLIterator; virtual;
    procedure CloseAllOpenWindows; virtual;

    (*--- event handling methods ---*)

    procedure OnIdle; virtual;
    procedure OnMousePressed (pos: Point); virtual;
    procedure OnMouseReleased(pos: Point); virtual;
    procedure OnMouseMove    (pos: Point); virtual;
    procedure OnKey(key: CHAR); virtual;
    procedure OnCommand(commandNr: INTEGER); virtual;

  end; (*OBJECT*)


var

  MLApplicationInstance: MLApplication; (*the one and only appl. object*)

  (*file menu:*)
  newCommand, closeCommand, quitCommand,
  (*edit menu:*)
  undoCommand, cutCommand, copyCommand, pasteCommand, clearCommand,
  (*window menu:*)
  closeAllCommand,
  (*help menue:*)
  aboutCommand
  : INTEGER;


function NewMLApplication(name: STRING): MLApplication;

procedure ShowMessageWindow(title, message: STRING);

function NewMenuCommand(menu, cmd: STRING; shortCut: CHAR): INTEGER;


(*======================================================================*)
implementation

uses
  MetaInfo;
    

procedure BuildFileAndEditMenus;
var
  dummy: INTEGER;
begin
  (*file menu:*)
  newCommand      := NewMenuCommand('File',   'New',       'N');
  closeCommand    := NewMenuCommand('File',   'Close',     'W');
  dummy           := NewMenuCommand('File',   '-',         ' ');
  quitCommand     := NewMenuCommand('File',   'Quit',      'Q');
  (*edit menu:*)
  undoCommand     := NewMenuCommand('Edit',   'Undo',      'Z');
  dummy           := NewMenuCommand('Edit',   '-',         ' ');
  cutCommand      := NewMenuCommand('Edit',   'Cut',       'X');
  copyCommand     := NewMenuCommand('Edit',   'Copy',      'C');
  pasteCommand    := NewMenuCommand('Edit',   'Paste',     'V');
  clearCommand    := NewMenuCommand('Edit',   'Clear',     ' ');
end; (*BuildFileAndEditMenus*)
    
procedure BuildWindowAndHelpMenus;
begin
  (*window menu: Cascade, Tile, and Arrang icons in OS_Bridge*)
  closeAllCommand := NewMenuCommand('Window', 'Close All', ' ');
  (*help menu:*)
  aboutCommand    := NewMenuCommand('Help',   'About ...', ' ');
end; (*BuildWindowAndHelpMenus*)


function NewMLApplication(name: STRING): MLApplication;
var
  a: MLApplication;
begin
  New(a, Init(name));
  NewMLApplication := a;
end; (*NewApplication*)


procedure ShowMessageWindow(title, message: STRING);
begin
  OSB_ShowMessageWindow(title, message);
end; (*ShowMessageWindow*)

function NewMenuCommand(menu, cmd: STRING;
  shortCut: CHAR): INTEGER;
begin
  NewMenuCommand := OSB_NewMenuCommand(menu, cmd, shortCut);
end; (*NewMenuCommand*)


(*=== class MLApplication ===*)


constructor  MLApplicationObj.Init(name: STRING);
begin
  inherited Init;
  REGISTER('MLApplication', 'MLObject');
  (*check if singleton*)
  if MLApplicationInstance = NIL then
    MLApplicationInstance := @SELF
  else
    Error('invalid attempt to construct another MLApplication object');
  (*reset golbal variables defined and initialized in MetaInfo*)
  applKind      := guiApplication;
  showMsgWindow := OSB_ShowMessageWindow;
  (*finish object initialization*)
  applName      := name;
  running       := TRUE;
  openWindows   := NewMLVector;
  (*initialize MS Windows application*)
  OSB_InitApplication(applName);
end; (*MLApplicationObj.Init*)


destructor MLApplicationObj.Done;
begin
  MLApplicationInstance := NIL;
  Dispose(openWindows, Done);
  OSB_DestroyApplication;
  inherited Done;
end; (*MLApplicationObj.Done*)


procedure MLApplicationObj.AddWindow(w: MLWindow);
begin
  openWindows^.Add(w);
end; (*MLApplicationObj.AddWindow*)
    
procedure MLApplicationObj.RemoveWindow(w: MLWindow);
begin
  openWindows^.Remove(w);
end; (*MLApplication.RemoveWindow*)

procedure MLApplicationObj.DispatchEvent(e: Event);
var
  w: MLWindow;
begin
  case e.kind of
    idleEvent: OnIdle; (*idleEvent*)
    buttonDownEvent: OnMousePressed(e.pos); (*buttonDownEvent*)
    buttonReleaseEvent: OnMouseReleased(e.pos); (*buttonReleaseEvent*)
    mouseMoveEvent: OnMouseMove(e.pos); (*mouseMoveEvent*)
    keyEvent: OnKey(e.key); (*keyEvent*)
    commandEvent: OnCommand(e.commandNr); (*commandEvent*)
    resizeEvent:
    begin
      w := WindowOf(e.wp);
      if w <> NIL then
        w^.OnResize;
    end; (*resizeEvent*)
    redrawEvent:
    begin
      w := WindowOf(e.wp);
      if w <>NIL then
        w^.OnRedraw;
    end; (*redrawEvent*)
    quitEvent: Quit; (*quitEvent*)
  else ; (*ELSE*)
    (*nothing to do*)end; (*CASE*)
end;(*MLApplicationObj.DispatchEvent*)


(*--- override to build application specific menus ---*)

procedure MLApplicationObj.BuildMenus;
begin
  (*implementation in derived classes calls NewMenuCommand*)
end; (*MLApplicationObj.BuildMenus*)


(*--- run and quit mehtods ---*)

procedure MLApplicationObj.Run;
var
  e: Event;
begin
  BuildFileAndEditMenus;
  BuildMenus;
  BuildWindowAndHelpMenus;
  OSB_InstallMenuBar;
  OpenNewWindow;
  OSB_GetNextEvent(e);
  while running do
  begin
    DispatchEvent(e);
    OSB_GetNextEvent(e);
  end; (*WHILE*)
  CloseAllOpenWindows;
end; (*MLApplicationObj.Run*)
    
procedure MLApplicationObj.Quit;
begin
  running := FALSE;
  OSB_RemoveMenuBar;
end; (*MLApplicationObj.Quit*)


(*--- window handling methods ---*)

procedure MLApplicationObj.OpenNewWindow;
begin
  NewMLWindow('MiniLib Window')^.Open;
end; (*MLApplicationObj.OpenNewWindow*)

function MLApplicationObj.WindowOf(wp: WindowPeer): MLWindow;
var
  it:    MLIterator;
  w:     MLWindow;
  found: BOOLEAN;
begin
  found := FALSE;
  it    := openWindows^.NewIterator;
  w     := MLWindow(it^.Next);
  while (not found) and (w <> NIL) do
    if OSB_EqualWindowPeers(w^.wp, wp) then
      found := TRUE
    else
      w := MLWindow(it^.Next); (*WHILE*)
  Dispose(it, Done);
  WindowOf := w;
end; (*MLApplicationObj.WindowOf*)
    
function MLApplicationObj.TopWindow: MLWindow;
begin
  TopWindow := WindowOf(OSB_ActiveWindowPeer);
end; (*MLApplicationObj.TopWindow*)

function MLApplicationObj.NewOpenWindowsIterator: MLIterator;
begin
  NewOpenWindowsIterator := openWindows^.NewIterator;
end; (*MLApplicationObj.NewOpenWindowsIterator*)

procedure MLApplicationObj.CloseAllOpenWindows;
var
  w: MLWindow;
  i: INTEGER;
begin
  i := openWindows^.Size;
  while i >= 1 do
  begin
    w := MLWindow(openWindows^.GetAt(i));
    Dispose(w, Done); (*removes w from openWindows, closes and deletes it*)
    i := i - 1;
  end; (*WHILE*)
end; (*MLApplicationObj.CloseAllOpenWindows*)
    

(*--- event handling methods ---*)

procedure MLApplicationObj.OnIdle;
var
  it: MLIterator;
  w:  MLWindow;
begin
  it := openWindows^.NewIterator;
  w  := MLWindow(it^.Next);
  while w <> NIL do
  begin
    w^.OnIdle;
    w := MLWindow(it^.Next);
  end; (*WHILE*)
  Dispose(it, Done);
end; (*MLApplicationObj.OnIdle*)

procedure MLApplicationObj.OnMousePressed(pos: Point);
var
  w: MLWindow;
begin
  w := TopWindow;
  if w <> NIL then
    w^.OnMousePressed(pos);
end; (*MLApplicationObj.OnMousePressed*)

procedure MLApplicationObj.OnMouseReleased(pos: Point);
var
  w: MLWindow;
begin
  w := TopWindow;
  if w <> NIL then
    w^.OnMouseReleased(pos);
end; (*MLApplicationObj.OnMouseReleased*)

procedure MLApplicationObj.OnMouseMove(pos: Point);
var
  w: MLWindow;
begin
  w := TopWindow;
  if w <> NIL then
    w^.OnMouseMove(pos);
end; (*MLApplicationObj.OnMouseMove*)
    
procedure MLApplicationObj.OnKey(key: CHAR);
var
  w: MLWindow;
begin
  w := TopWindow;
  if w <> NIL then
    w^.OnKey(key);
end; (*MLApplicationObj.OnKey*)

procedure MLApplicationObj.OnCommand(commandNr: INTEGER);
var
  w: MLWindow;
begin
  if commandNr = quitCommand then
    Quit
  else if commandNr = closeAllCommand then
    CloseAllOpenWindows
  else if commandNr = aboutCommand then
    OSB_ShowMessageWindow('About Window', applName +
      CHR(10) + CHR(13) + 'based on the famous application framework MiniLib' +
      CHR(10) + CHR(13) + '-- Version 4.0 (in Pascal), 2004')
  else if commandNr = newCommand then
    OpenNewWindow
  else begin (*delegate to top window*)
    w := TopWindow;
    if w <> NIL then
      w^.OnCommand(commandNr);
  end; (*ELSE*)
end; (*MLApplicationObj.OnCommand*)


begin (*MLAppl*)
  MLApplicationInstance := NIL;
end. (*MLAppl*)
 
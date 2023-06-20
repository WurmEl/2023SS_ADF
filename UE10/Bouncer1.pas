(*Bouncer1:                                            MiniLib V.4, 2004
  --------
  Bouncing ball application.
  Version 1: without ball interaction.
========================================================================*)
program Bouncer1;

uses
  MetaInfo, OSBridge,
  MLObj, MLWin, MLAppl;

type
  BouncerWindow = ^BouncerWindowObj;
  BouncerWindowObj = object(MLWindowObj)
    pos: Point;
    dx, dy, size, speed: INTEGER;
    constructor Init(title: STRING);
    (*overridden methods*)
    procedure Open; virtual;
    procedure Redraw; virtual;
    procedure OnIdle; virtual;
    procedure OnCommand(commandNr: INTEGER); virtual;
    (*new methods*)
    procedure InvertBall; virtual;
    procedure ChangeSize(newSize: INTEGER); virtual;
    procedure ChangeSpeed(newSpeed: INTEGER); virtual;
  end; (*OBJECT*)

  BouncerApplication= ^BouncerApplicationObj;
  BouncerApplicationObj = object(MLApplicationObj)
    constructor Init(name: STRING);
    (*overridden methods*)
    procedure OpenNewWindow; virtual;
    procedure BuildMenus; virtual;
  end; (*OBJECT*)

var
  (*size menu:*)
  smallCommand, mediumCommand, largeCommand: INTEGER;
  (*speed menu:*)
  crawlCommand, walkCommand, runCommand, flyCommand: INTEGER;


(*=== BouncerWindow ===*)

function NewBouncerWindow: BouncerWindow;
var
  w: BouncerWindow;
begin
  New(w, Init('Bouncer Window'));
  NewBouncerWindow := w;
end; (*NewBouncerWindow*)

constructor BouncerWindowObj.Init(title: STRING);
begin
  inherited Init(title);
  Register('BouncerWindow', 'MLWindow');
  pos.x :=  0;
  pos.y :=  0;
  size  := 10;
  speed := 10;
  dx    := speed;
  dy    := speed;
end; (*BouncerWindowObj.Init*)

procedure BouncerWindowObj.Open;
begin
  inherited Open;
  InvertBall;
end; (*BouncerWindowObj.Open*)

procedure BouncerWindowObj.Redraw;
begin
  InvertBall;
end; (*BouncerWindowObj.Redraw*)

procedure BouncerWindowObj.OnIdle;

  procedure Move(var val, delta: INTEGER; max: INTEGER);
  begin
    val := val + delta;
    if val < 0 then
    begin
      val   := 0;
      delta := + speed;
    end (*THEN*)
    else if val + size > max then
    begin
      val   := max - size;
      delta := -speed;
    end; (*ELSE*)
  end; (*Move*)

begin (*BouncerWindowObj.OnIdle*)
  InvertBall;
  Move(pos.x, dx, Width);
  Move(pos.y, dy, Height);
  InvertBall;
end; (*BouncerWindowObj.OnIdle*)

procedure BouncerWindowObj.OnCommand(commandNr: INTEGER);
begin
  writeln('here3');
  if commandNr = smallCommand then
    ChangeSize(10) 
  else if commandNr = mediumCommand then
    ChangeSize(20)
  else if commandNr = largeCommand then
    ChangeSize(40)
  else if commandNr = crawlCommand then
    ChangeSpeed(1)
  else if commandNr = walkCommand then
    ChangeSpeed(5)
  else if commandNr = runCommand then
    ChangeSpeed(10)
  else if commandNr = flyCommand then
    ChangeSpeed(20)
  else
    inherited OnCommand(commandNr);
end; (*BouncerWindowObj.OnCommand*)

procedure BouncerWindowObj.InvertBall;
begin
  DrawFilledOval(pos, size, size);
end; (*BouncerWindowObj.InvertBall*)

procedure BouncerWindowObj.ChangeSize(newSize: INTEGER);
begin
  InvertBall;
  size := newSize;
  InvertBall;
end; (*BouncerWindowObj.ChangeSize*)

procedure BouncerWindowObj.ChangeSpeed(newSpeed: INTEGER);
begin
  speed := newSpeed;
  if dx < 0 then
    dx := -speed
  else
    dx := speed;
  if dy < 0 then
    dy := -speed
  else
    dy := speed;
end; (*BouncerWindowObj.ChangeSpeed*)


(*=== BouncerApplication ===*)

function NewBouncerApplication: BouncerApplication;
var
  a: BouncerApplication;
begin
  New(a, Init('Bouncer Application V.1'));
  NewBouncerApplication := a;
end; (*NewBouncerApplication*)

constructor BouncerApplicationObj.Init(name: STRING);
begin
  inherited Init(name);
  Register('BouncerApplication', 'MLApplication');
end; (*BouncerApplicationObj.Init*)

procedure BouncerApplicationObj.OpenNewWindow;
begin
  NewBouncerWindow^.Open;
end; (*BouncerApplicationObj.OpenNewWindow*)

procedure BouncerApplicationObj.BuildMenus;
begin
  (*size menu:*)
  smallCommand  := NewMenuCommand('Size',  'Small',  'S');
  mediumCommand := NewMenuCommand('Size',  'Medium', 'M');
  largeCommand  := NewMenuCommand('Size',  'Large',  'L');
  (*speed menu:*)
  crawlCommand  := NewMenuCommand('Speed', 'Crawl',  '1');
  walkCommand   := NewMenuCommand('Speed', 'Walk',   '2');
  runCommand    := NewMenuCommand('Speed', 'Run',    '3');
  flyCommand    := NewMenuCommand('Speed', 'Fly',    '4');
end; (*BouncerApplicationObj.BuildMenus*)


(*=== main program ===*)

var
  a: BouncerApplication;

begin (*Bouncer1*)
  a := NewBouncerApplication;
  a^.Run;
  Dispose(a, Done);
  WriteMetaInfo;
end. (*Bouncer1*)

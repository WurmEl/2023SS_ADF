unit TabStringBuilderUnit;

interface

uses
  StringBuilderUnit;

type
  TabStringBuilderPtr = ^TabStringBuilderObj;
  TabStringBuilderObj = object(StringBuilderObj)
  public
    constructor Init(width: integer);
    destructor Done; virtual;
    procedure AppendStr(e: string); virtual;
    procedure AppendChar(e: char); virtual;
    procedure AppendInt(e: integer); virtual;
    procedure AppendBool(e: boolean); virtual;
  private
    columnWidth: integer;
    function AlignText(text: string): string;
  end;

function NewTabStringBuilder(width: integer): TabStringBuilderPtr;

implementation

function NewTabStringBuilder(width: integer): TabStringBuilderPtr;
var
  builder: TabStringBuilderPtr;
begin
  New(builder, Init(width));
  NewTabStringBuilder := builder;
end;

constructor TabStringBuilderObj.Init(width: integer);
begin
  inherited Init;
  columnWidth := width;
end;

destructor TabStringBuilderObj.Done;
begin
  inherited done;
end;

procedure TabStringBuilderObj.AppendStr(e: string);
begin
  inherited AppendStr(AlignText(e));
end;

procedure TabStringBuilderObj.AppendChar(e: char);
begin
  inherited AppendStr(AlignText(e));
end;

procedure TabStringBuilderObj.AppendInt(e: integer);
var
  intStr: string;
begin
  Str(e, intStr);
  inherited AppendStr(AlignText(intStr));
end;

procedure TabStringBuilderObj.AppendBool(e: boolean);
begin
  if e then
    inherited AppendStr(AlignText('TRUE'))
  else
    inherited AppendStr(AlignText('FALSE'));
end;

function TabStringBuilderObj.AlignText(text: string): string;
var
  temp: string;
begin
  if Length(text) >= columnWidth then
    temp := Copy(text, 1, columnWidth)
  else
  begin
    temp := text;
    while Length(temp) < columnWidth do
      temp := Concat(temp, ' ');
  end;
  AlignText := temp;
end;

end.

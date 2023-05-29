unit StringBuilderUnit;

interface

type
  StringBuilderPtr = ^StringBuilderObj;
  StringBuilderObj = object
  public
    constructor Init;
    destructor Done; virtual;

    procedure AppendStr(e: string); virtual;
    procedure AppendChar(e: char); virtual;
    procedure AppendInt(e: integer); virtual;
    procedure AppendBool(e: boolean); virtual;
    function AsString: string; virtual;
    function BufferLength: integer;
  private
    buffer: string;
  end;

function NewStringBuilder: StringBuilderPtr;

implementation

function NewStringBuilder: StringBuilderPtr;
var
  builder: StringBuilderPtr;
begin
  New(builder, init);
  NewStringBuilder := builder;
end;

constructor StringBuilderObj.Init;
begin
  buffer := '';
end;

destructor StringBuilderObj.Done;
begin
end;

procedure StringBuilderObj.AppendStr(e: string);
begin
  buffer := buffer + e;
end;

procedure StringBuilderObj.AppendChar(e: char);
begin
  buffer := buffer + e;
end;

procedure StringBuilderObj.AppendInt(e: integer);
var
  intStr: string;
begin
  Str(e, intStr);
  buffer := buffer + intStr;
end;

procedure StringBuilderObj.AppendBool(e: boolean);
begin
  if e then
    buffer := buffer + 'TRUE'
  else
    buffer := buffer + 'FALSE';
end;

function StringBuilderObj.AsString: string;
begin
  AsString := buffer;
end;

function StringBuilderObj.BufferLength: integer;
begin
  BufferLength := Length(buffer);
end;

end.

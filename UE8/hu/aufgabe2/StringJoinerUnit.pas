unit StringJoinerUnit;

interface

uses
  StringBuilderUnit;

type
  StringJoinerPtr = ^StringJoinerObj;
  StringJoinerObj = object
  public
    constructor Init(delimiter: char);
    destructor Done; virtual;

    procedure Add(e: string);
    function AsString: string;
  private
    delimiter: char;
    count: integer;
    resultBuilder: StringBuilderPtr;
  end;

function NewStringJoiner(delimiter: char): StringJoinerPtr;

implementation

function NewStringJoiner(delimiter: char): StringJoinerPtr;
var
  joiner: StringJoinerPtr;
begin
  New(joiner, Init(delimiter));
  NewStringJoiner := joiner;
end;

constructor StringJoinerObj.Init(delimiter: char);
begin
  self.delimiter := delimiter;
  count := 0;
  resultBuilder := NewStringBuilder;
end;

destructor StringJoinerObj.Done;
begin
  Dispose(resultBuilder, Done);
end;

procedure StringJoinerObj.Add(e: string);
begin
  if count > 0 then
    resultBuilder^.AppendChar(delimiter);
  resultBuilder^.AppendStr(e);
  Inc(count);
end;

function StringJoinerObj.AsString: string;
begin
  AsString := resultBuilder^.AsString;
end;

end.

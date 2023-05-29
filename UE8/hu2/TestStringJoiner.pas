program TestStringJoiner;

uses
  StringJoinerUnit;

procedure ExeuteStringJoinerTests;
var
  joiner: StringJoinerPtr;
begin
  // Create a StringJoiner with delimiter ","
  joiner := NewStringJoiner(',');

  // Add some strings
  joiner^.Add('Hello');
  joiner^.Add('World');
  joiner^.Add('!');
  joiner^.Add('How');
  joiner^.Add('are');
  joiner^.Add('');
  joiner^.Add('you');
  joiner^.Add('today');
  joiner^.Add('?');

  // Get and print the result
  Writeln('Result: ', joiner^.AsString);

  // Clean up memory
  Dispose(joiner, done);
end;

begin
  ExeuteStringJoinerTests;
end.

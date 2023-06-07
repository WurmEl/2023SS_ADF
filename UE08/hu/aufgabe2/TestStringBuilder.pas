program TestStringBuilder;

uses
  StringBuilderUnit, TabStringBuilderUnit;

procedure ExecuteStringBuilderTests(builder: StringBuilderPtr);
begin
  // Append different types of values to the StringBuilder
  builder^.AppendStr('Hello ');
  builder^.AppendChar('W');
  builder^.AppendChar('o');
  builder^.AppendChar('r');
  builder^.AppendChar('l');
  builder^.AppendChar('d');
  builder^.AppendInt(2023);
  builder^.AppendBool(true);
  builder^.AppendStr('123456789');

  // Get the resulting string from the StringBuilder
  Writeln('StringBuilder content: ', builder^.AsString);
end;

var
  myBuilder: StringBuilderPtr;
  myTabBuilder: TabStringBuilderPtr;

begin
  myBuilder := NewStringBuilder;
  myTabBuilder := NewTabStringBuilder(8);

  Writeln('Testing StringBuilder:');
  ExecuteStringBuilderTests(myBuilder);

  Dispose(myBuilder, Done);

  writeln; writeln;
  Writeln('Testing TabStringBuilder:');
  ExecuteStringBuilderTests(myTabBuilder);

  Dispose(myTabBuilder, Done);
  writeln; writeln;
end.

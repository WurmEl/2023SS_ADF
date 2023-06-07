program StackTest;

uses ArrayStackUnit, SaveArrayStackUnit, FlexibleArrayStackUnit, ListStackUnit, StackUnit;

procedure TestStack(stack: StackPtr; title: string);
var
  i: integer;
begin
  writeln('testing ', title);
  writeln('push 1 to 30 into stack');
  for i := 1 to 30 do
    stack^.push(i);

  writeln('remove until empty');
  while(not stack^.isEmpty) do
  begin
    stack^.pop(i);
    writeln(i);
  end;

  Dispose(stack, done);
end;

begin
  TestStack(NewListStack, 'ListStack');
end.

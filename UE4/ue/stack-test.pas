(* Stack ADS:                                         Elias Wurm, 2023-04-17 *)
(* ------                                                                    *)
(* Fun with abstract data structures and types for stacks                    *)
(* ========================================================================= *)
program StackTest;

uses StackUnit;

var
  value, i: integer;
  s: Stack;
begin
  s := NewStack;
  for i := 1 to 10 do Push(s, i);

  while(not IsEmpty(s)) do
  begin
    Pop(s, value);
    writeln(value);
  end;

  DisposeStack(s);
end.

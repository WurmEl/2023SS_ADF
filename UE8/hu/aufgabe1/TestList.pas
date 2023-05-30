program TestList;

uses
  ListUnit, SortedListUnit;

procedure ExecuteListTests(l: List);
begin
  // Add some values to the list
  l^.Add(5);
  l^.Add(10);
  l^.Add(15);

  // Print the size of the list
  Writeln('Size of the list: ', l^.Size);

  // Check if the list contains a value
  Writeln('List contains 10: ', l^.Contains(10));
  Writeln('List contains 20: ', l^.Contains(20));

  // Remove a value from the list
  l^.Remove(10);

  // Print the updated size of the list
  Writeln('Size of the list after removal: ', l^.Size);

  // Clear the list
  l^.Clear;
  Writeln('Size of the list after clearing: ', l^.Size);
end;

var
  l: List;
  sl: SortedList;

begin
  l := NewList;
  sl := NewSortedList;

  Writeln('Testing List:');
  ExecuteListTests(l);
  Dispose(l, Done);

  writeln;writeln;  
  Writeln('Testing SortedList:');
  ExecuteListTests(sl);
  Dispose(sl, Done);
  writeln;writeln;
end.

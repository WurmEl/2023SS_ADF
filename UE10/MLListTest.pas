program MLListTest;

uses MLLi, MLObj, MLInt, MLColl, MetaInfo;

procedure RunMLListTests;
var
  list: MLList;
  int2: MLInteger;
  iterator: MLIterator;
  next: MLObject;
begin
  list := NewMLList;

  int2 := NewMlInt(2);

  list^.Add(NewMLInt(1));
  list^.Add(int2);
  list^.Add(NewMLInt(3));

  writeln('Size: ', list^.Size); // Output: 3

  writeln('Removed int2: ', list^.Remove(int2)^.AsString);

  writeln('Contains int2: ', list^.Contains(int2)); // Output: False
  Dispose(int2, Done);

  iterator := list^.NewIterator;
  next := iterator^.Next;

  while next <> NIL do
  begin
    writeln('Iterator value: ', next^.asString); // Output: 1, 3
    next := iterator^.Next;
  end;
  writeln;
  Dispose(iterator, Done);

  list^.Prepend(NewMLInt(4));

  iterator := list^.NewIterator;
  next := iterator^.Next;

  while next <> NIL do
  begin
    writeln('Iterator value: ', next^.asString); // Output: 4, 1, 3
    next := iterator^.Next;
  end;
  writeln;
  Dispose(iterator, Done);

  list^.Clear;

  writeln('Size: ', list^.Size); // Output: 0

  Dispose(list, Done);
end;

begin
  RunMLListTests;
  WriteMetaInfo;
end.

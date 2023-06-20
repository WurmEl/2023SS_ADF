program MLPrg;
  uses MLStr, MetaInfo, MLInt;

var
  i: MLInteger;

begin
  i := NewMlInt(16);
  i^.WriteAsString;
  writeln;
  writeln(i^.AsInteger + 4);
  Dispose(i, Done);
  WriteMetaInfo;
end.
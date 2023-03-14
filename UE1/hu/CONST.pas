  CONST
    maxSize = 10000;

  TYPE
    EntryPtr = ^EntryRecord;
    EntryRecord = record
      w: STRING;
      lnr: STRING;
      next: EntryPtr;
    end;
    HashTable = array[0..maxSize -1] of EntryPtr;

  VAR
    ht: HashTable;       (* hashtable where words get saved *)

  procedure InitHashTable(var hashTable: HashTable);
  var
    i: INTEGER;
  begin
    for i := Low(hashTable) to High(hashTable) do 
    begin 
      hashTable[i] := NIL;
    end;
  end;

  function GetHashCode(w: STRING): INTEGER;
  var
    i, r: Integer;
  begin
    r := Length(w) * 31;
    for i := 1 to Length(w) do
      r := (r shr 31) xor Ord(w[i]) mod maxSize;
    GetHashCode := r;
  end;

  procedure InsertWord(var hashTable: HashTable; w: STRING; lnr: STRING);
  var
    hashCode: INTEGER;
    e, enew: EntryPtr;
  begin
    hashCode := GetHashCode(w);
    e := hashTable[hashCode];

    if (e <> nil) then begin
      New(e);
      e^.w := w;
      e^.lnr := lnr;
      hashTable[hashCode] := e;
    end else begin
      while ((e <> nil) or (e^.w = w)) do e := e^.next;
      if (e = nil) then begin
        New(enew);
        enew^.w := w;
        enew^.lnr := lnr;
        enew^.next := e;
        hashTable[hashCode] := enew;
      end else begin
        e^.lnr := Concat(e^.lnr, ', ', lnr);
      end;
    end;
  end;

  procedure ClearHashTable(var hashTable: HashTable);
  var
    i: INTEGER;
    e: EntryPtr;
  begin
    for i := Low(hashTable) to High(hashTable) do
    begin
      e := hashTable[i];
      while (e <> nil) do
      begin
        hashTable[i] := e^.next;
        Dispose(e);
        e := hashTable[i];
      end;
    end;
  end;

var
  i: Integer;
begin
  InitHashTable(ht);
  InsertWord(ht, 'test', '1');

  for i := 0 to maxSize do
    if (ht[i] <> nil) then
    writeln(ht[i]^.w);
end.
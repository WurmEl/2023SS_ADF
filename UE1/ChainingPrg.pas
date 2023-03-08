PROGRAM ChainingPrg;
  CONST
    maxSize = 100;

  TYPE
    EntryPtr = ^EntryRecord;
    EntryRecord = RECORD
      data: STRING;
      next: EntryPtr;
    END;
    HashTable = ARRAY[0..maxSize -1] OF EntryPtr;

  VAR
    ht: HashTable;

  procedure InitHashTable(VAR hashTable: HashTable);
    VAR
      i: INTEGER;
  BEGIN
    FOR i := Low(hashTable) TO High(hashTable) DO BEGIN
      hashTable[i] := NIL;
    END;
  END;

  function GetHashCode(data: STRING): INTEGER;
  BEGIN
    if(Length(data) = 0) THEN BEGIN
      GetHashCode := 0;
    END else if (Length(data) = 1) THEN BEGIN
      GetHashCode := (Ord(data[1]) * 7 + 1) * 17;
    END else BEGIN
      GetHashCode := (Ord(data[1]) * 7 + Ord(data[2]) + Length(data)) * 17;
    END;
  END;

  function NewEntry(data: STRING; next: EntryPtr): EntryPtr;
    VAR
      e: EntryPtr;
  BEGIN
    New(e);
    e^.data := data;
    e^.next := next;
    NewEntry := e;
  END;

  procedure Insert(VAR hashTable: HashTable; data: STRING);
    VAR
      hashCode: INTEGER;
  BEGIN
    hashCode := GetHashCode(data) MOD maxSize;
    hashTable[hashCode] := NewEntry(data, hashTable[hashCode]);
  END;

  function Contains(hashTable: HashTable; data: STRING): BOOLEAN;
    VAR
      hashCode: INTEGER;
      e: EntryPtr;
  BEGIN
    hashCode := GetHashCode(data) MOD maxSize;
    e := hashTable[hashCode];
    WHILE ((e <> nil) AND (e^.data <> data)) DO BEGIN
      e := e^.next;
    END;

    Contains := e <> NIL;
  END;

  procedure ClearHashTable(VAR hashTable: HashTable);
    var
      i: INTEGER;
      e: EntryPtr;
  begin
    FOR i := Low(hashTable) TO High(hashTable) DO BEGIN
      e := hashTable[i];
      while (e <> nil) DO BEGIN
        hashTable[i] := e^.next;
        Dispose(e);
        e := hashTable[i];
    END;
  end;

  
BEGIN
  InitHashTable(ht);
  Insert(ht, 'Stefan');
  Insert(ht, 'Test');
  Insert(ht, 'ASDAS');
  Insert(ht, 'df32');

  WriteLn('Contains(Stefan): ', Contains(ht, 'Stefan'));
  WriteLn('Contains(Stefan): ', Contains(ht, 'Franz'));

  ClearHashTable(ht);
END.

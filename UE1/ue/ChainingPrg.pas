(* Hashing Chaining Program:                          Wurm Elias, 2023-03-09 *)
(* ------                                                                    *)
(* Uses Chaining to Hash data                                                *)
(* ========================================================================= *)

program ChainingPrg;
const
  maxSize = 100;

type
  EntryPtr = ^EntryRecord;
  EntryRecord = record
    data: STRING;
    next: EntryPtr;
  end;
  HashTable = array[0..maxSize -1] of EntryPtr;

var
  ht: HashTable;

procedure InitHashTable(var hashTable: HashTable);
var
  i: INTEGER;
begin
  for i := Low(hashTable) to High(hashTable) do hashTable[i] := NIL;
end;

function GetHashCode(data: STRING): INTEGER;
begin
  if(Length(data) = 0) then GetHashCode := 0 else if (Length(data) = 1) then GetHashCode := (Ord(data[1]) * 7 + 1) * 17 else GetHashCode := (Ord(data[1]) * 7 + Ord(data[2]) + Length(data)) * 17;
end;

function NewEntry(data: STRING; next: EntryPtr): EntryPtr;
var
  e: EntryPtr;
begin
  New(e);
  e^.data := data;
  e^.next := next;
  NewEntry := e;
end;

procedure Insert(var hashTable: HashTable; data: STRING);
var
  hashCode: INTEGER;
begin
  hashCode := GetHashCode(data) mod maxSize;
  hashTable[hashCode] := NewEntry(data, hashTable[hashCode]);
end;

function Contains(hashTable: HashTable; data: STRING): BOOLEAN;
var
  hashCode: INTEGER;
  e: EntryPtr;
begin
  hashCode := GetHashCode(data) mod maxSize;
  e := hashTable[hashCode];
  while ((e <> nil) and (e^.data <> data)) do e := e^.next;

  Contains := e <> NIL;
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
  
begin
  InitHashTable(ht);
  Insert(ht, 'Stefan');
  Insert(ht, 'Test');
  Insert(ht, 'ASDAS');
  Insert(ht, 'df32');

  WriteLn('Contains(Stefan): ', Contains(ht, 'Stefan'));
  WriteLn('Contains(Franz): ', Contains(ht, 'Franz'));

  ClearHashTable(ht);
end.

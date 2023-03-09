(* Hashing Probing Program:                           Wurm Elias, 2023-03-09 *)
(* ------                                                                    *)
(* Uses Probing to Hash data                                                 *)
(* ========================================================================= *)

program ProbingPrg;
const
  maxSize = 100;

type 
  EntryPtr = ^EntryRecord; 
  EntryRecord = record
    data: STRING;
    deleted: BOOLEAN;
  end;
  HashTable = array[0..maxSize -1] of EntryPtr;

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

function NewEntry(data: STRING): EntryPtr;
var
  e: EntryPtr;
begin
  New(e);
  e^.data := data;
  e^.deleted := false;
  NewEntry := e;
end;

procedure Insert(var hashTable: HashTable; data: STRING);
var
  hashCode, offset, i: INTEGER;
begin
  hashCode := GetHashCode(data) mod maxSize;
  offset := 0;
  (* quadratic probing with offset² with offset only its linear probing *)
  i := hashCode + (offset * offset) mod maxSize;
    
  while ((hashTable[i] <> nil) and not hashTable[i]^.deleted) do
  begin
    offset := offset + 1;
    i := hashCode + (offset * offset) mod maxSize;

    if(offset > maxSize) then
    begin
      WriteLn('HashTable is full!');
      Halt;
    end;
  end;

  if(hashTable[i] = NIL) then hashTable[i] := NewEntry(data) else begin
    hashTable[i]^.data := data;
    hashTable[i]^.deleted := false;
  end;
end;

function Contains(hashTable: HashTable; data: STRING): BOOLEAN;
var
  hashCode, offset, i: INTEGER;
begin
  hashCode := GetHashCode(data) mod maxSize;
  offset := 0;
  i := hashCode + (offset * offset) mod maxSize;
  while ((hashTable[i] <> nil) 
      and ((hashTable[i]^.data <> data) or hashTable[i]^.deleted) 
      and (offset <= maxSize)) do
  begin
    offset := offset + 1;
    i := hashCode + (offset * offset) mod maxSize;
  end;

  Contains := (hashTable[i] <> NIL) and (offset <= maxSize);
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
      Dispose(e);
      hashTable[i] := nil;
    end;
  end;
end;

var
  ht: HashTable;

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

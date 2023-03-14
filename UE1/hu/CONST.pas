program test;
const
  maxSize = 100;
  maxWordLen = 30;

type
  Word = STRING[maxWordLen];
  EntryPtr = ^EntryRecord; 
  EntryRecord = record
    word: Word;
    lnr: STRING;
    deleted: BOOLEAN;
  end;
  HashTable = array[0..maxSize -1] of EntryPtr;

procedure InitHashTable(var hashTable: HashTable);
var
  i: INTEGER;
begin
  for i := Low(hashTable) to High(hashTable) do hashTable[i] := NIL;
end;

function GetHashCode(w: Word): INTEGER;
var
  i, r: Integer;
begin
  r := Length(w) * 31;
  for i := 1 to Length(w) do
    r := (r shr 31) xor Ord(w[i]) mod maxSize;
  GetHashCode := r;
end;

function NewEntry(word: Word; lnr: STRING): EntryPtr;
var
  e: EntryPtr;
begin
  New(e);
  e^.word := word;
  e^.lnr := lnr;
  e^.deleted := false;
  NewEntry := e;
end;

function Contains(hashTable: HashTable; word: Word): BOOLEAN;
var
  hashCode, offset, i: INTEGER;
begin
  hashCode := GetHashCode(word) mod maxSize;
  offset := 0;
  i := hashCode + (offset * offset) mod maxSize;
  while ((hashTable[i] <> nil) 
      and ((hashTable[i]^.word <> word) or hashTable[i]^.deleted) 
      and (offset <= maxSize)) do
  begin
    offset := offset + 1;
    i := hashCode + (offset * offset) mod maxSize;
  end;

  Contains := (hashTable[i] <> NIL) and (offset <= maxSize);
end;

procedure Insert(var hashTable: HashTable; word: Word; lnr: STRING);
var
  hashCode, offset, i: INTEGER;
begin
  hashCode := GetHashCode(word) mod maxSize;
  offset := 0;
  i := hashCode + (offset * offset) mod maxSize;

  if(Contains(hashTable, word)) then 
  begin
    hashTable[i]^.lnr := Concat(hashTable[i]^.lnr,', ', lnr);
    writeln(hashTable[i]^.word);
    writeln(hashTable[i]^.lnr);
  end else begin
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

    if(hashTable[i] = NIL) then hashTable[i] := NewEntry(word, lnr) else begin
      hashTable[i]^.word := word;
      hashTable[i]^.lnr := lnr;
      hashTable[i]^.deleted := false;
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
      Dispose(e);
      hashTable[i] := nil;
    end;
  end;
end;

var 
  ht: HashTable;
begin
  InitHashTable(ht);
  Insert(ht, 'test', '1');
  writeln(Contains(ht, 'test'));
  writeln(Contains(ht, 'test1'));
  Insert(ht, 'test', '2');
  ClearHashTable(ht);
end.

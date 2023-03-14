(* UniqueSubstr:                                      Wurm Elias, 2023-03-09 *)
(* ------                                                                    *)
(* Description: reads a string from the standard input and calculates the    *)
(* number of unique substrings of length L contained in it. To do this, it   *)
(* stores the unique substrings in a hash table. For example, for the input  *)
(* ABABABB and length L = 3, there are three                                 *)
(* unique substrings: ABA, BAB, and ABB.                                     *)
(* ========================================================================= *)

program UniqueSubstr;
const
  maxSize = 2048;

type
  EntryPtr = ^EntryRecord;
  EntryRecord = record
    data: STRING;
    next: EntryPtr;
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

function LengthHT(hashTable: HashTable): integer;
var
  i, result: integer;
  e: EntryPtr;
begin
  result := 0;
  for i := Low(hashTable) to High(hashTable) do
  begin
    e := hashTable[i];
    while (e <> nil) do
    begin
      Inc(result);
      e := e^.next;
    end;
  end;
  LengthHT := result;
end;

var
  inputStr: string;
  L, i, j: integer;
  substr: string;
  uniqueSubstrings: HashTable;
begin
  InitHashTable(uniqueSubstrings);
  j := 0;

  writeln('Enter length of unique substrings to count for:');
  readln(L);
  writeln('Enter text:');
  readln(inputStr);

  for i := 1 to Length(inputStr) - L + 1 do
  begin
    substr := Copy(inputStr, i, L);
    if not Contains(uniqueSubstrings, substr) then
    begin
      Inc(j);
      Insert(uniqueSubstrings, substr);
    end;
  end;

  writeln('Number of unique substrings of length ', L, ': ', LengthHT(uniqueSubstrings));

  ClearHashTable(uniqueSubstrings);
end.

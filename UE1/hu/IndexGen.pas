(* IndexGen:                              Wurm Elias, 2023-03-14 *)
(* --------                                                      *)
(* Generation of a sorted index of all words in a text file      *)
(* with Hashtables.                                              *)
(* ============================================================= *)
PROGRAM IndexGen;
  USES
    Timer, Sysutils;

  CONST
    ef = Chr(0);       (* end of file character *)
    maxWordLen = 30;   (* max. number of characters per word *)
    chars = ['a' .. 'z', 'ä', 'ö', 'ü', 'ß',
             'A' .. 'Z', 'Ä', 'Ö', 'Ü']; 
    maxSize = 32767;

  TYPE
    Word = STRING[maxWordLen];
    EntryPtr = ^EntryRecord; 
    EntryRecord = record
      word: Word;
      lnr: STRING; 
      deleted: BOOLEAN;
    end;
    HashTable = array[0..maxSize -1] of EntryPtr;

  VAR
    txt: TEXT;           (* text file *)
    curLine: STRING;     (* current line from file txt *)
    curCh: CHAR;         (* current character *)
    curLineNr: INTEGER;  (* current line number *)
    curColNr: INTEGER;   (* current column number *)
    ht: HashTable;       (* hashtable where words get saved *)

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

  FUNCTION LowerCase(ch: CHAR): STRING;
  BEGIN (* LowerCase *)
    CASE ch OF
      'A'..'Z': LowerCase := Chr(Ord(ch) + (Ord('a') - Ord('A')));
      'Ä', 'ä': LowerCase := 'ae';
      'Ö', 'ö': LowerCase := 'oe';
      'Ü', 'ü': LowerCase := 'ue';
      'ß':      LowerCase := 'ss';
      ELSE (* all the others *)
                LowerCase := ch;
      END; (* CASE *)
  END; (* LowerCase *)

  PROCEDURE GetNextChar; (* updates curChar, ... *)
  BEGIN (* GetNextChar *)
    IF (curColNr < Length(curLine)) THEN BEGIN
      curColNr := curColNr + 1;
      curCh := curLine[curColNr]
    END ELSE BEGIN (* curColNr >= Length(curLine) *)
      IF NOT Eof(txt) THEN BEGIN
        ReadLn(txt, curLine);
        curLineNr:= curLineNr + 1;
        curColNr := 0;
        curCh := ' '; (* separate lines by ' ' *)
      END ELSE BEGIN (* Eof(txt) *)
        curCh := EF;
      END; (* IF *)
    END; (* IF *)
  END; (* GetNextChar *)

  PROCEDURE GetNextWord(VAR w: Word; VAR lnr: INTEGER);
  BEGIN (* GetNextWord *)
    WHILE (curCh <> ef) AND NOT (curCh IN chars) DO BEGIN
      GetNextChar;
    END; (* WHILE *)
    lnr := curLineNr;
    IF curCh <> ef THEN BEGIN
      w := LowerCase(curCh);
      GetNextChar;
      WHILE (curCh <> ef) AND (curCh IN chars) DO BEGIN
        w := Concat(w , LowerCase(curCh));
        GetNextChar;
      END; (* WHILE *)
    END ELSE BEGIN (* curCh = ef *)
      w := '';
    END; (* IF *)
  END; (* GetNextWord *)


  PROCEDURE HashTableToSortedArray(table: HashTable);
  VAR
    entries: array of EntryPtr;
    dummy: EntryPtr;
    i, j, n: Integer;
    sorted: Boolean;
    outputFile: TextFile;
  BEGIN
    Assign(outputFile, 'outputFile.txt');
    Rewrite(outputFile);

    SetLength(entries, maxSize);
    n := 0;
    for i := 0 to High(table) do BEGIN
      if (table[i] <> nil) and (not table[i]^.deleted) then BEGIN
        entries[n] := table[i];
        Inc(n);
      END;
    END;
    SetLength(entries, n);

    repeat
      sorted := True;
      for i := 0 to n-2 do BEGIN
        if entries[i]^.word > entries[i+1]^.word then BEGIN
          dummy := entries[i];
          entries[i] := entries[i+1];
          entries[i+1] := dummy;
          sorted := False;
        end;
      end;
      Dec(n);
    until sorted;

    for i := 0 to High(entries) do BEGIN
      writeln(outputFile, Concat(entries[i]^.word, ' ', entries[i]^.lnr));
    END;

    Close(outputFile);
  END;

  VAR
    txtName: STRING;
    w: Word;        (* current word *)
    lnr: INTEGER;   (* line number of current word *)
    n: LONGINT;     (* number of words *)

BEGIN (* IndexGen *)
  InitHashTable(ht);

  Write('IndexGen: index generation for text file ');

  IF ParamCount = 0 THEN BEGIN
    WriteLn;
    WriteLn; 
    Write('name of text file > ');
    ReadLn(txtName);
  END ELSE BEGIN
    txtName := ParamStr(1);
    WriteLn(txtName);
  END; (* IF *)
  WriteLn;

  (* --- read text from text file --- *)
  Assign(txt, txtName);
  Reset(txt);
  curLine := '';
  curLineNr := 0;
  curColNr := 1; (* curColNr > Length(curLine) forces reading of first line *)
  GetNextChar;   (* curCh now holds first character *)

  // StartTimer;
  GetNextWord(w, lnr);
  n := 0;
  WHILE Length(w) > 0 DO BEGIN
    Insert(ht, w, IntToStr(lnr));
    n := n + 1;
    GetNextWord(w, lnr);
  END; (* WHILE *)
  // StopTimer;

  // WriteLn;
  // WriteLn('number of words: ', n);
  // WriteLn('elapsed time:    ', ElapsedTime);

  HashTableToSortedArray(ht);

  Close(txt);
  ClearHashTable(ht);
END. (* IndexGen *)
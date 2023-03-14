(* IndexGen:                                     HDO, 2002-02-28 *)
(* --------                                                      *)
(* Generation of a sorted index of all words in a text file.     *)
(* Command line:                                                 *)
(*    IndexGen [ textFileName ]                                  *)
(* ============================================================= *)
PROGRAM IndexGen;
  USES
    Timer, sysutils;

  CONST
    ef = Chr(0);       (* end of file character *)
    maxWordLen = 30;   (* max. number of characters per word *)
    chars = ['a' .. 'z', '�', '�', '�', '�',
             'A' .. 'Z', '�', '�', '�'];   
    maxSize = 10000;

  TYPE
    Word = STRING[maxWordLen];
    EntryPtr = ^EntryRecord;
    EntryRecord = record
      w: STRING;
      lnr: STRING;
      next: EntryPtr;
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
    hashCode, i: INTEGER;
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

  FUNCTION LowerCase(ch: CHAR): STRING;
  BEGIN (* LowerCase *)
    CASE ch OF
      'A'..'Z': LowerCase := Chr(Ord(ch) + (Ord('a') - Ord('A')));
      '�', '�': LowerCase := 'ae';
      '�', '�': LowerCase := 'oe';
      '�', '�': LowerCase := 'ue';
      '�':      LowerCase := 'ss';
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

  StartTimer;
  GetNextWord(w, lnr);
  n := 0;
  WHILE Length(w) > 0 DO BEGIN
    WriteLn(w, ' ', lnr);
    InsertWord(ht, w, IntToStr(lnr));
    n := n + 1;
    GetNextWord(w, lnr);
  END; (* WHILE *)
  StopTimer;

  WriteLn;
  WriteLn('number of words: ', n);
  WriteLn('elapsed time:    ', ElapsedTime);
  Close(txt);
  
  ReadLn;
  
END. (* IndexGen *)
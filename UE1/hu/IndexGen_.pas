(* IndexGen:                                     HDO, 2002-02-28 *)
(* --------                                                      *)
(* Generation of a sorted i of all words in a text file.     *)
(* Command line:                                                 *)
(*    IndexGen [ textFileName ]                                  *)
(* ============================================================= *)
PROGRAM IndexGenHash;
  USES
    Timer;

  CONST
    ef = Chr(0);       (* end of file character *)
    maxWordLen = 30;   (* max. number of characters per word *)
    chars = ['a' .. 'z', '�', '�', '�', '�', 'A' .. 'Z', '�', '�', '�'];    
    tableSize = 50000; (* size of hash table *)
  
  TYPE
    Word = STRING[maxWordLen];
    HashTable = ARRAY[0 .. tableSize-1] OF RECORD
                  key: Word;
                  count: INTEGER;
                END;

  VAR
    txt: TEXT;           (* text file *)
    curLine: STRING;     (* current line from file txt *)
    curCh: CHAR;         (* current character *)
    curLineNr: INTEGER;  (* current line number *)
    curColNr: INTEGER;   (* current column number *)
    hashTable: HashTable; (* hash table *)

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

  FUNCTION HashFunction(w: Word): INTEGER;
  VAR
    i: INTEGER;
    hash: INTEGER;
  BEGIN
    hash := 5381;
    FOR i := 1 TO Length(w) DO BEGIN
      hash := ((hash SHL 5) + hash) + Ord(w[i]);
    END;
    HashFunction := hash MOD tableSize;
  END;

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

  PROCEDURE AddWord(word: Word);
  VAR
    i: INTEGER;
  BEGIN
    i := HashFunction(word, tableSize);
    WHILE (hashTable[i].word <> '') AND (hashTable[i].word <> word) DO BEGIN
      i := (i + 1) MOD tableSize;
    END;
    IF hashTable[i].word = word THEN BEGIN
      hashTable[i].count := hashTable[i].count + 1;
    END ELSE BEGIN
      hashTable[i].word := word;
      hashTable[i].count := 1;
    END;
  END;

VAR
    txtName: STRING;
    w: Word;        (* current word *)
    lnr, i: INTEGER;   (* line number of current word *)
    n: LONGINT;     (* number of words *)

BEGIN (* IndexGen *)

  (* Init Hashtable *)
  FOR i := 0 TO tableSize-1 DO BEGIN
    hashTable[i].count := 0;
  END;

  Write('IndexGen: i generation for text file ');

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
    AddWord(w);
    n := n + 1;
    GetNextWord(w, lnr);
  END; (* WHILE *)
  StopTimer;

  WriteLn;
  WriteLn('number of words: ', n);
  WriteLn('elapsed time:    ', ElapsedTime);
  Close(txt);

  FOR i := 0 TO tableSize-1 DO BEGIN
    IF hashtable[i].count > 0 THEN BEGIN
      WriteLn(hashtable[i].word, ' ', hashtable[i].count);
    END;
  END;
  
  ReadLn;
  
END. (* IndexGen *)
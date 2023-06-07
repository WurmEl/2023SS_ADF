(* MP_Lex:                                                   SWa, 2023-04-26 *)
(* -------                                                                   *)
(* Scanner for MiniPascal.                                                   *)
(* ========================================================================= *)
UNIT MP_Lex;

INTERFACE
  TYPE
    Symbol = (
      noSy, errSy, eofSy,
      programSy, beginSy, endSy, varSy, integerSy, readSy, writeSy,
      semicolonSy, periodSy, commaSy, colonSy, assignSy,
      plusSy, minusSy, timesSy, divSy, leftParSy, rightParSy,
      identSy, numberSy
    );
  VAR
    sy: Symbol;            (* current symbol *)
    syLnr, syCnr: INTEGER; (* current start position of sy *)
    identStr: STRING;      (* identifier if sy = identSy *)
    numberVal: INTEGER;    (* number if sy = numberSy *)
  
  PROCEDURE InitScanner(filename: STRING; VAR ok: BOOLEAN);
  PROCEDURE NewSy;         (* read next symbol from input *)

IMPLEMENTATION

  CONST
    eofCh = Chr(0);
    tabCh = Chr(9);

  VAR
    textfile: TEXT;
    line: STRING;
    ch: CHAR;
    chLnr, chCnr: INTEGER;

  PROCEDURE NewCh;
  BEGIN (* NewCh *)
    IF (chCnr < Length(line)) THEN BEGIN
      chCnr := chCnr + 1;
      ch := line[chCnr];
    END ELSE BEGIN
      IF (NOT Eof(textfile)) THEN BEGIN
        ReadLn(textfile, line);
        chLnr := chLnr + 1;
        chCnr := 0;
        ch := ' ';  (* to separate two lines *)
      END ELSE BEGIN
        Close(textfile);
        ch := eofCh;
      END; (* IF *)
    END; (* IF *)
  END; (* NewCh *)

  PROCEDURE InitScanner(filename: STRING; VAR ok: BOOLEAN);
  BEGIN (* InitScanner *)
    Assign(textfile, filename);
    {$I-}
    Reset(textFile);
    {$I+}
    ok := IOResult = 0;
    IF (ok) THEN BEGIN
      line := '';
      chLnr := 0;
      chCnr := 1;
      NewCh;
      NewSy;
    END; (* IF *)
  END; (* InitScanner *)

  PROCEDURE NewSy;
  BEGIN (* InitScanner *)
    sy := noSy;
    REPEAT
      WHILE ((ch = ' ') OR (ch = tabCh)) DO BEGIN
        NewCh;
      END; (* WHILE *)
      syLnr := chLnr;
      syCnr := chCnr;
      CASE ch OF
        eofCh: BEGIN sy := eofSy;              END;
        '+':   BEGIN sy := plusSy; NewCh;      END;
        '-':   BEGIN sy := minusSy; NewCh;     END;
        '*':   BEGIN sy := timesSy; NewCh;     END;
        '/':   BEGIN sy := divSy; NewCh;       END;
        '(':   BEGIN sy := leftParSy; NewCh;   END;
        ')':   BEGIN sy := rightParSy; NewCh;  END;
        ';':   BEGIN sy := semicolonSy; NewCh; END;
        '.':   BEGIN sy := periodSy; NewCh;    END;
        ',':   BEGIN sy := commaSy; NewCh;     END;
        ':':   BEGIN
                 sy := colonSy; NewCh;
                 IF (ch = '=') THEN BEGIN
                   sy := assignSy; NewCh;
                 END; (* IF *)
               END;

        'a'..'z', 'A'..'Z': BEGIN
                              identStr := '';
                              WHILE ch IN ['a'..'z', 'A'..'Z', '_', '0'..'9'] DO BEGIN
                                identStr := identStr + UpCase(ch);
                                NewCh;
                              END; (* WHILE *)

                              IF (identStr = 'PROGRAM') THEN BEGIN
                                sy := programSy;
                              END ELSE IF (identStr = 'BEGIN') THEN BEGIN
                                sy := beginSy;
                              END ELSE IF (identStr = 'END') THEN BEGIN
                                sy := endSy;
                              END ELSE IF (identStr = 'VAR') THEN BEGIN
                                sy := varSy;
                              END ELSE IF (identStr = 'READ') THEN BEGIN
                                sy := readSy;
                              END ELSE IF (identStr = 'WRITE') THEN BEGIN
                                sy := writeSy;
                              END ELSE IF (identStr = 'INTEGER') THEN BEGIN
                                sy := integerSy;
                              END ELSE BEGIN
                                sy := identSy;
                              END; (* IF *)
                            END;
   
        '0'..'9': BEGIN
                    sy := numberSy;
                    numberVal := 0;
                    WHILE ((ch >= '0') AND (ch <= '9')) DO BEGIN
                      numberVal := numberVal * 10 + Ord(ch) - Ord('0');
                      NewCh;
                    END; (* WHILE *)
                  END;

        ELSE   BEGIN sy := errSy;              END;
      END; (* CASE *)
      
    UNTIL (sy <> noSy); (* REPEAT *)
  END; (* InitScanner *)

  


END. (* MP_Lex *)
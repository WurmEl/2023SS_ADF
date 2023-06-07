(* MPP_S:                                                    SWa, 2023-05-03 *)
(* ------                                                                    *)
(* MiniPascal parser.                                                        *)
(* ========================================================================= *)
UNIT MPP_S;

INTERFACE
  
  VAR
    success: BOOLEAN;

  PROCEDURE S;

IMPLEMENTATION

  USES
    MP_Lex;

  FUNCTION SyIsNot(expectedSy: Symbol): BOOLEAN;
  BEGIN (* SyIsNot *)
    success := success AND (sy = expectedSy);
    SyIsNot := NOT success;
  END; (* SyIsNot *)

  PROCEDURE MP;      FORWARD;
  PROCEDURE VarDecl; FORWARD;
  PROCEDURE StatSeq; FORWARD;
  PROCEDURE Stat;    FORWARD;
  PROCEDURE Expr;    FORWARD;
  PROCEDURE Term;    FORWARD;
  PROCEDURE Fact;    FORWARD;

  PROCEDURE S;
  BEGIN (* S *)
    WriteLn('Parsing started ...');
    success := TRUE;
    MP;
    IF ((NOT success) OR SyIsNot(eofSy)) THEN BEGIN
      WriteLn('ERROR: Error in line ', syLnr, ' and column ', syCnr);
    END ELSE BEGIN
      WriteLn('... parsing completed successfully.')
    END; (* IF *)
  END; (* S *)

  PROCEDURE MP;
  BEGIN (* MP *)
    IF (SyIsNot(programSy)) THEN Exit;
    NewSy;
    IF (SyIsNot(identSy)) THEN Exit;
    NewSy;
    IF (SyIsNot(semicolonSy)) THEN Exit;
    NewSy;
    IF (sy = varSy) THEN BEGIN
      VarDecl; IF (NOT success) THEN Exit;
    END; (* IF *)
    IF (SyIsNot(beginSy)) THEN Exit;
    NewSy;
    StatSeq; IF (NOT success) THEN Exit;
    IF (SyIsNot(endSy)) THEN Exit;
    NewSy;
    IF (SyIsNot(periodSy)) THEN Exit;
    NewSy;
  END; (* MP *)

  PROCEDURE VarDecl;
  BEGIN (* VarDecl *)
    IF (SyIsNot(varSy)) THEN Exit;
    NewSy;
    IF (SyIsNot(identSy)) THEN Exit;
    NewSy;
    WHILE (sy = commaSy) DO BEGIN
      NewSy;
      IF (SyIsNot(identSy)) THEN Exit;
      NewSy;
    END; (* WHILE *)
    IF (SyIsNot(colonSy)) THEN Exit;
    NewSy;
    IF (SyIsNot(integerSy)) THEN Exit;
    NewSy;
    IF (SyIsNot(semicolonSy)) THEN Exit;
    NewSy;
  END; (* VarDecl *)

  PROCEDURE StatSeq;
  BEGIN (* StatSeq *)
    Stat; IF (NOT success) THEN Exit;
    WHILE (sy = semicolonSy) DO BEGIN
      NewSy;
      Stat; IF (NOT success) THEN Exit;
    END; (* WHILE *)
  END; (* StatSeq *)

  PROCEDURE Stat;
  BEGIN (* Stat *)
    CASE sy OF
      identSy: BEGIN
                 NewSy;
                 IF (SyIsNot(assignSy)) THEN Exit;
                 NewSy;
                 Expr; IF (NOT success) THEN Exit;
               END;
      readSy:  BEGIN
                 NewSy;
                 IF (SyIsNot(leftParSy)) THEN Exit;
                 NewSy;
                 IF (SyIsNot(identSy)) THEN Exit;
                 NewSy;
                 IF (SyIsNot(rightParSy)) THEN Exit;
                 NewSy;
               END;
      writeSy: BEGIN
                 NewSy;
                 IF (SyIsNot(leftParSy)) THEN Exit;
                 NewSy;
                 Expr; IF (NOT success) THEN Exit;
                 IF (SyIsNot(rightParSy)) THEN Exit;
                 NewSy;
               END;
    END; (* CASE *)
  END; (* Stat *)

  PROCEDURE Expr;
  BEGIN (* Expr *)
    Term; IF NOT success THEN Exit;
    WHILE ((sy = plusSy) OR (sy = minusSy)) DO BEGIN
      CASE sy OF
        plusSy:  BEGIN
                   NewSy;
                   Term; IF NOT success THEN Exit;
                 END;
        minusSy: BEGIN
                   NewSy;
                   Term; IF NOT success THEN Exit;
                 END;
      END; (* CASE *)
    END; (* WHILE *)
  END; (* Expr *)

  PROCEDURE Term;
  BEGIN (* Term *)
    Fact; IF NOT success THEN Exit;
    WHILE ((sy = timesSy) OR (sy = divSy)) DO BEGIN
      CASE sy OF
        timesSy: BEGIN
                   NewSy;
                   Fact; IF NOT success THEN Exit;
                 END;
        divSy:   BEGIN
                   NewSy;
                   Fact; IF NOT success THEN Exit;
                 END;
      END; (* CASE *)
    END; (* WHILE *)
  END; (* Term *)

  PROCEDURE Fact;
  BEGIN (* Fact *)
    CASE sy OF
      identSy:   BEGIN
                   NewSy;
                 END;
      numberSy:  BEGIN
                   NewSy;
                 END;
      leftParSy: BEGIN
                   NewSy;
                   Expr; IF NOT success THEN Exit;
                   IF (SyIsNot(rightParSy)) THEN Exit;
                   NewSy;
                 END;
      ELSE       BEGIN
                   success := FALSE;
                 END;
    END; (* CASE *)
  END; (* Fact *)

END. (* MPP_S *)

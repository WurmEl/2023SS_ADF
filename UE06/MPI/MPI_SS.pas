(* MPI_SS:                                                   SWa, 2023-05-03 *)
(* ------                                                                    *)
(* MiniPascal interpreter.                                                   *)
(* ========================================================================= *)
UNIT MPI_SS;

INTERFACE
  
  VAR
    success: BOOLEAN;

  PROCEDURE S;

IMPLEMENTATION

  USES
    MP_Lex, SymTab;

  FUNCTION SyIsNot(expectedSy: Symbol): BOOLEAN;
  BEGIN (* SyIsNot *)
    success := success AND (sy = expectedSy);
    SyIsNot := NOT success;
  END; (* SyIsNot *)

  PROCEDURE SemErr(message: STRING);
  BEGIN (* SemErr *)
    WriteLn('SEMANTIC ERROR: ', message);
    success := FALSE;
  END; (* SemErr *)

  PROCEDURE MP;      FORWARD;
  PROCEDURE VarDecl; FORWARD;
  PROCEDURE StatSeq; FORWARD;
  PROCEDURE Stat;    FORWARD;
  PROCEDURE Expr(VAR e: INTEGER); FORWARD;
  PROCEDURE Term(VAR t: INTEGER); FORWARD;
  PROCEDURE Fact(VAR f: INTEGER); FORWARD;

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
    (* SEM *) InitSymbolTable; (* ENDSEM *)
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
    VAR
      ok: BOOLEAN;
  BEGIN (* VarDecl *)
    IF (SyIsNot(varSy)) THEN Exit;
    NewSy;
    IF (SyIsNot(identSy)) THEN Exit;
    (* SEM *) DeclVar(identStr, ok); (* ENDSEM *)
    NewSy;
    WHILE (sy = commaSy) DO BEGIN
      NewSy;
      IF (SyIsNot(identSy)) THEN Exit;
      (* SEM *)
        DeclVar(identStr, ok);
        IF (NOT ok) THEN BEGIN
          SemErr('variable already declared');
        END; (* IF *)
      (* ENDSEM *)
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
    VAR
      destId: STRING;
      e: INTEGER;
  BEGIN (* Stat *)
    CASE sy OF
      identSy: BEGIN
                 (* SEM *)
                   destId := identStr;
                   IF (NOT IsDecl(destId)) THEN BEGIN
                     SemErr('variable not declared');
                   END; (* IF *)
                 (* ENDSEM *)
                 NewSy;
                 IF (SyIsNot(assignSy)) THEN Exit;
                 NewSy;
                 Expr(e); IF (NOT success) THEN Exit;
                 (* SEM *)
                   IF (IsDecl(destId)) THEN BEGIN
                     SetVal(destId, e);
                   END; (* IF *)
                 (* ENDSEM *)
               END;
      readSy:  BEGIN
                 NewSy;
                 IF (SyIsNot(leftParSy)) THEN Exit;
                 NewSy;
                 IF (SyIsNot(identSy)) THEN Exit;
                 (* SEM *)
                   IF (NOT IsDecl(identStr)) THEN BEGIN
                     SemErr('variable not declared');
                   END ELSE BEGIN
                     Write(identStr, ' > ');
                     ReadLn(e);
                     SetVal(identStr, e);
                   END; (* IF *)
                 (* ENDSEM *)
                 NewSy;
                 IF (SyIsNot(rightParSy)) THEN Exit;
                 NewSy;
               END;
      writeSy: BEGIN
                 NewSy;
                 IF (SyIsNot(leftParSy)) THEN Exit;
                 NewSy;
                 Expr(e); IF (NOT success) THEN Exit;
                 (* SEM *) WriteLn(e); (* ENDSEM *)
                 IF (SyIsNot(rightParSy)) THEN Exit;
                 NewSy;
               END;
    END; (* CASE *)
  END; (* Stat *)

  PROCEDURE Expr(VAR e: INTEGER);
    VAR
      t: INTEGER;
  BEGIN (* Expr *)
    Term(e); IF NOT success THEN Exit;
    WHILE ((sy = plusSy) OR (sy = minusSy)) DO BEGIN
      CASE sy OF
        plusSy:  BEGIN
                   NewSy;
                   Term(t); IF NOT success THEN Exit;
                   (* SEM *) e := e + t; (* ENDSEM *)
                 END;
        minusSy: BEGIN
                   NewSy;
                   Term(t); IF NOT success THEN Exit;
                   (* SEM *) e := e - t; (* ENDSEM *)
                 END;
      END; (* CASE *)
    END; (* WHILE *)
  END; (* Expr *)

  PROCEDURE Term(VAR t: INTEGER);
    VAR
      f: INTEGER;
  BEGIN (* Term *)
    Fact(t); IF NOT success THEN Exit;
    WHILE ((sy = timesSy) OR (sy = divSy)) DO BEGIN
      CASE sy OF
        timesSy: BEGIN
                   NewSy;
                   Fact(f); IF NOT success THEN Exit;
                   (* SEM *) t := t * f; (* ENDSEM *)
                 END;
        divSy:   BEGIN
                   NewSy;
                   Fact(f); IF NOT success THEN Exit;
                   (* SEM *)
                     IF (f = 0) THEN BEGIN
                       SemErr('division by zero');
                     END ELSE BEGIN
                       t := t DIV f;
                     END; (* IF *)
                   (* ENDSEM *)
                 END;
      END; (* CASE *)
    END; (* WHILE *)
  END; (* Term *)

  PROCEDURE Fact(VAR f: INTEGER);
  BEGIN (* Fact *)
    CASE sy OF
      identSy:   BEGIN
                   (* SEM *)
                     IF (NOT IsDecl(identStr)) THEN BEGIN
                       SemErr('variable not declared');
                     END ELSE BEGIN
                       GetVal(identStr, f);
                     END; (* IF *)
                   (* ENDSEM *)
                   NewSy;
                 END;
      numberSy:  BEGIN
                   (* SEM *) f := numberVal; (* ENDSEM *)
                   NewSy;
                 END;
      leftParSy: BEGIN
                   NewSy;
                   Expr(f); IF NOT success THEN Exit;
                   IF (SyIsNot(rightParSy)) THEN Exit;
                   NewSy;
                 END;
      ELSE       BEGIN
                   success := FALSE;
                 END;
    END; (* CASE *)
  END; (* Fact *)

END. (* MPI_SS *)

(* MPC_SS:                                                   SWa, 2023-05-03 *)
(* ------                                                                    *)
(* MiniPascal compiler.                                                        *)
(* ========================================================================= *)
UNIT MPC_SS;

INTERFACE
  
  VAR
    success: BOOLEAN;

  PROCEDURE S;

IMPLEMENTATION

  USES
    MP_Lex, SymTab, CodeDef, CodeGen;

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
  PROCEDURE Expr;    FORWARD;
  PROCEDURE Term;    FORWARD;
  PROCEDURE Fact;    FORWARD;

  PROCEDURE S;
  BEGIN (* S *)
    WriteLn('parsing started ...');
    success := TRUE;
    MP;
    IF ((NOT success) OR SyIsNot(eofSy)) THEN BEGIN
      WriteLn('ERROR: Error in line ', syLnr, ' and column ', syCnr);
    END ELSE BEGIN
      WriteLn('... parsing ended')
    END; (* IF *)
  END; (* S *)

  PROCEDURE MP;
  BEGIN (* MP *)
    (* SEM *)
      InitSymbolTable;
      InitCodeGenerator;
    (* ENDSEM *)
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
    (* SEM *) Emit1(EndOpc); (* ENDSEM *)
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
  BEGIN (* Stat *)
    CASE sy OF
      identSy: BEGIN
                 (* SEM *)
                   destId := identStr;
                   IF (NOT IsDecl(destId)) THEN BEGIN
                     SemErr('variable not declared');
                    END ELSE BEGIN
                      Emit2(LoadAddrOpc, AddrOf(destId));
                    END; (* IF *)
                 (* ENDSEM *)
                 NewSy;
                 IF (SyIsNot(assignSy)) THEN Exit;
                 NewSy;
                 Expr; IF (NOT success) THEN Exit;
                 (* SEM *)
                   IF (IsDecl(destId)) THEN BEGIN
                     Emit1(StoreOpc);
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
                     Emit2(ReadOpc, AddrOf(identStr));
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
                 Expr; IF (NOT success) THEN Exit;
                 (* SEM *) Emit1(WriteOpc); (* ENDSEM *)
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
                   (* SEM *) Emit1(AddOpc); (* ENDSEM *)
                 END;
        minusSy: BEGIN
                   NewSy;
                   Term; IF NOT success THEN Exit;
                   (* SEM *) Emit1(SubOpc); (* ENDSEM *)
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
                   (* SEM *) Emit1(MulOpc); (* ENDSEM *)
                 END;
        divSy:   BEGIN
                   NewSy;
                   Fact; IF NOT success THEN Exit;
                   (* SEM *) Emit1(DivOpc); (* ENDSEM *)
                 END;
      END; (* CASE *)
    END; (* WHILE *)
  END; (* Term *)

  PROCEDURE Fact;
  BEGIN (* Fact *)
    CASE sy OF
      identSy:   BEGIN
                   (* SEM *)
                     IF (NOT IsDecl(identStr)) THEN BEGIN
                       SemErr('variable not declared');
                     END ELSE BEGIN
                       Emit2(LoadValOpc, AddrOf(identStr));
                     END; (* IF *)
                   (* ENDSEM *)
                   NewSy;
                 END;
      numberSy:  BEGIN
                   (* SEM *) Emit2(LoadConstOpc, numberVal); (* ENDSEM *)
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

END. (* MPC_SS *)

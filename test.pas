(* :Calculator                                        Fleischanderl, 2023-04-19 *)
(* ----calcs simple expressions                      *)
(* ========================================================================= *)
program Calculator;
const
  eofCh = Chr(0);

type
  Symbol = (
    eofSy,
    errSy,
    plusSy, minusSy, timesSy, divSy, 
    leftParSy, rightParSy, 
    numberSy
    );

var
  line: STRING;       (* input sequence *)
  ch: CHAR;           (* current character *)
  chNr: INTEGER;      (* pos of ch *)
  sy: Symbol;         (* current symbol *)
  numberVal: INTEGER; (* numerical value IF sy is a numberSy *)
  success: BOOLEAN;   (* syntax correct *)

(* SCANNER *)
procedure NewChar;
begin
  if(chNr < Length(line)) then
  begin
    Inc(chNr);
    ch := line[chNr];
  end else ch := eofCh;
end;

procedure NewSy;
begin
  while(ch = ' ') do NewChar;
  case ch of
    eofCh: sy := eofSy;
    '+':
    begin sy := plusSy; NewChar; end;
    '-':
    begin sy := minusSy; NewChar; end;
    '*':
    begin sy := timesSy; NewChar; end;
    '/':
    begin sy := divSy; NewChar; end;
    '(':
    begin sy := leftParSy; NewChar; end;
    ')':
    begin sy := rightParSy; NewChar; end;
    '0'..'9':
    begin 
      sy := numberSy;
      numberval := 0;
      while((ch >= '0') and (ch <= '9')) do
      begin
        numberval := numberVal * 10 + Ord(ch) - Ord('0');
        NewChar;
      end;
    end;
  else
    sy := errSy;    
  end;
end;

(* Parser *)

procedure S;                    forward;
procedure Expr(var e: INTEGER); forward;
procedure Term(var t: INTEGER); forward;
procedure Fact(var f: INTEGER); forward;

procedure S;
var
  e: INTEGER;
begin
  Expr(e); if not success then exit;
  // SEM
  writeln(e);
  // ENDSEM
  if(sy <> eofSy) then
  begin
    success := false;
    exit;
  end;
end;

procedure Expr(var e: INTEGER);
var
  t: INTEGER;
begin
  Term(e);
  if not success then exit;
  while(sy = plusSy) or (sy = minusSy) do
    case sy of
      plusSy:
      begin
        NewSy;
        Term(t); 
        if not success then exit;
        // SEM
        write('+',t,e);
        // ENDSEM
      end;
      minusSy:
      begin 
        NewSy;
        Term(t); if not success then exit;
        // SEM
        write('-',t,e);
        // ENDSEM
      end;
    end;
end;

procedure Term(var t: INTEGER);
var
  f: INTEGER;
begin
  Fact(t); 
  if not success then exit;
  while(sy = timesSy) or (sy = divSy) do
    case sy of
      timesSy:
      begin
        NewSy;
        Fact(f); 
        if not success then exit;
        // SEM
        write('*',f,t);
        // ENDSEM
      end;
      divSy:
      begin 
        NewSy;
        Fact(f); 
        if not success then exit;
        // SEM
        write('/',f,t);
        // ENDSEM
      end;
    end;
end;

procedure Fact(var f: INTEGER);
begin
  case sy of
    numberSy:
    begin NewSy; (*SEM*)f:= numberVal;(*ENDSEM*)end;
    leftParSy: 
    begin 
      NewSy;
      Expr(f);
      if not success then exit;
      if(sy <> rightParSy) then
      begin 
        success := false; Exit; 
      end;
      NewSy;
    end;
  else
    success := false;
  end;
end;

(* Main *)
begin
  write('expr > '); readln(line);
  while(line <> '') do
  begin
    chNr := 0;
    NewChar;
    NewSy;
    success := true;
    S;
    if not success then writeln('syntax error');
    write('expr > '); 
    readln(line);
  end; // WHILE
end.

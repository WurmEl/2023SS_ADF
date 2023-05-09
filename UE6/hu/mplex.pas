(* MiniPascalLexikalischerAnalysator:                 Elias Wurm, 2023-04-26 *)
(* ------                                                                    *)
(* Scanner for Mini Pascal                                                   *)
(* ========================================================================= *)

unit MPLex;

interface

type
  Symbol = (
    emptySy, eofSy, errSy,
    numberSy, identSy,
    semicolonSy, colonSy, commaSy, periodSy, assignSy,
    plusSy, minusSy, timesSy, divSy, 
    leftParSy, rightParSy, 
    programSy,
    varSy, integerSy,
    ifSy, elseSy, thenSy, whileSy, doSy,
    readSy, writeSy,
    beginSy, endSy
    );

var
  sy: Symbol;
  syLnr, syCnr: integer;
  numberVal: integer;
  identStr: string;

procedure NewSy;
procedure InitScanner(filename: string; var success: boolean);

implementation

const
  eofCh = chr(0);
  tabCh = chr(9);

var
  textfile: TEXT;
  line: string;
  ch: char;
  chCNr, chLNr: integer;

procedure NewCh;
begin
  if(chCNr < Length(line)) then
  begin
    Inc(chCnr);
    ch := line[chCnr];
  end else if(not eof(textfile)) then
  begin
    readln(textfile, line);
    Inc(chLnr);
    chCnr := 0;
    ch := ' '; (* to seperate two lines *)
  end else begin
    Close(textfile);
    ch := eofCh;
  end;
end;

procedure InitScanner(filename: string; var success: boolean);
begin
  assign(textfile, filename);
  {$I-}
  reset(textfile);
  {$I+}
  success := IOResult = 0;
  if(success) then
  begin
    line := '';
    chCNr := 0;
    chLNr := 1;
    NewCh;
    NewSy;
  end;
end;

procedure NewSy;
begin
  sy := emptySy;
  repeat
    while((ch = ' ') or (ch = tabCh)) do NewCh;

    syLnr := chLnr;
    syCnr := chCNr;

    case ch of
      eofCh: sy := eofSy;
      '+':
      begin sy := plusSy; NewCh; end;
      '-':
      begin sy := minusSy; NewCh; end;
      '*':
      begin sy := timesSy; NewCh; end;
      '/':
      begin sy := divSy; NewCh; end;
      '(':
      begin sy := leftParSy; NewCh; end;
      ')':
      begin sy := rightParSy; NewCh; end;
      ';':
      begin sy := semicolonSy; NewCh; end;
      ':':
      begin 
        sy := colonSy; NewCh;
        if(ch = '=') then
        begin
          sy := assignSy; NewCh;
        end;
      end;
      '.':
      begin sy := periodSy; NewCh; end;
      ',':
      begin sy := commaSy; NewCh; end;
      '0'..'9':
      begin 
        sy := numberSy;
        numberval := 0;
        while((ch >= '0') and (ch <= '9')) do
        begin
          numberval := numberVal * 10 + Ord(ch) - Ord('0');
          NewCh;
        end;
      end;
      'a'..'z', 'A'..'Z', '_':
      begin
        identStr := '';
        while((ch in ['a'..'z','A'..'Z','_','0'..'9'])) do
        begin
          identStr := identStr + UpCase(ch);
          NewCh;
        end;
        if(identStr = 'PROGRAM') then 
          sy := programSy
        else if(identStr = 'VAR') then 
          sy := varSy
        else if(identStr = 'READ') then 
          sy := readSy
        else if(identStr = 'WRITE') then 
          sy := writeSy
        else if(identStr = 'BEGIN') then 
          sy := beginSy
        else if(identStr = 'END') then 
          sy := endSy
        else if(identStr = 'INTEGER') then 
          sy := integerSy
        else if(identStr = 'IF') then 
          sy := ifSy
        else if(identStr = 'ELSE') then 
          sy := elseSy
        else if(identStr = 'THEN') then 
          sy := thenSy
        else if(identStr = 'WHILE') then 
          sy := whileSy
        else if(identStr = 'DO') then 
          sy := doSy
        else sy := identSy;
      end;

    else sy := errSy;
    end;

  until(sy <> emptySy);
end;

end.

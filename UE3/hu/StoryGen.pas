(* StoryGen:                                          Elias Wurm, 2023-04-12 *)
(* ------                                                                    *)
(* transforms stories by replacing specific words                            *)
(* ========================================================================= *)

program StoryGen;

uses SysUtils;

const
  MAX_REPL_SIZE = 1000;

type
  Repl = record
    OldWord: string;
    NewWord: string;
  end;

var
  repls: array[1..MAX_REPL_SIZE] of Repl;
  replsSize: integer;

procedure CheckIfFileExists(fileName: string);
begin
  if not FileExists(fileName) then
  begin
    WriteLn('Error: file does not exist - ', fileName);
    writeln;
    Halt;
  end;
end;

procedure CheckFilenamesIdentical(file1, file2: string);
begin
  if (file1 = file2) then
  begin
    WriteLn('Error: file can not be the same - ', file1);
    writeln;
    Halt;
  end; 
end;

function GetFilename(paramInt: Integer; msg: string): string;
var
  fileName: string;
begin
  if ParamCount > (paramInt - 1) then
    fileName := ParamStr(paramInt)
  else begin
    write(msg, ' > ');
    ReadLn(fileName);
  end;
  GetFilename := fileName;
end;

procedure getReplFromStr(str: string; var repl: Repl);
var
  whitespacePos: Integer;
begin
  whitespacePos := Pos(' ', str);
  
  if(whitespacePos = 0) then
  begin
    WriteLn('Error: incorrect format of replacements file');
    writeln;
    Halt;
  end;

  repl.OldWord := Copy(str, 1, whitespacePos - 1);
  repl.NewWord := Copy(str, whitespacePos + 1, Length(str) - whitespacePos);
end;

procedure readRepls(fileName: string);
var
  replFile: TEXT;
  line: string;
  foundRepl: Repl;
begin
  assign(replFile, fileName);
  reset(replFile);

  replsSize := 0;

  while not eof(replFile) do
  begin
    readln(replFile, line);

    getReplFromStr(line, foundRepl);
    Inc(replsSize);

    repls[replsSize].OldWord := foundRepl.OldWord;
    repls[replsSize].NewWord := foundRepl.NewWord;
  end;
 
  close(replFile);
end;

procedure openFiles(var inFile, outFile: TEXT; inFileName, outFileName: string);
begin
  Assign(inFile, inFileName);
  Reset(inFile);
  Assign(outFile, outFileName);
  Rewrite(outFile);
end;

procedure closeFiles(var inFile, outFile: TEXT);
begin
  Close(inFile);
  Close(outFile);
end;

procedure replaceWords(var line: string);
var
  i, wPos: integer;
begin
  for i := 1 to (replsSize) do
  begin
    wPos := Pos(repls[i].OldWord, line);
    while(wPos <> 0) do
    begin
      Delete(line, wPos, Length(repls[i].OldWord));
      Insert(repls[i].NewWord, line, wPos);
      wPos := Pos(repls[i].OldWord, line);
    end;
  end;
end;

procedure runReplacements(inFileName, outFileName: string);
var
  line: string;
  inFile, outFile: TEXT;
begin
  openFiles(inFile, outFile, inFileName, outFileName);

  while(not Eof(inFile)) do
  begin
    ReadLn(inFile, line);
    replaceWords(line);
    writeln(outFile, line);
  end;

  closeFiles(inFile, outFile);
end;

var
  replsFileName, inFileName, outFileName: string;
begin
  replsFileName := GetFilename(1, 'Enter fileName with the replacements');
  CheckIfFileExists(replsFileName);

  inFileName := GetFilename(2, 'enter text infilename');
  CheckIfFileExists(inFileName);
  CheckFilenamesIdentical(inFileName, replsFileName);

  outFileName := GetFilename(3, 'enter text outfilename');
  CheckFilenamesIdentical(outFileName, replsFileName);
  CheckFilenamesIdentical(outFileName, inFileName);

  readRepls(replsFileName);
  runReplacements(inFileName, outFileName);
end.

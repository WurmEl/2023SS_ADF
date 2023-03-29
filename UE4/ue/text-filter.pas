(* TextFilter:                                        Wurm Elias, 2023-03-29 *)
(* ------                                                                    *)
(* Simple Text Filter Program to filter textfies                             *)
(* ========================================================================= *)

program TextFilter;
type
  FileMode = (readFile, writeFile);

procedure CheckIOError(msg: STRING);
var
  errorCode: INTEGER;
begin
  errorCode := IOResult;
  if(errorCode <> 0) then
    writeln('ERROR ', errorCode,' :', msg);
end;

procedure CloseTextFile(var textFile: TEXT);
begin
  Close(textFile);
  CheckIOError('An other oopsie happend!');
end;

procedure OpenTextFile(var textfile: TEXT; filename: STRING; mode: FileMode);
begin
  Assign(textfile, filename);
  {$I-}
  if(mode = readFile) then
    Reset(textfile)
  else if(mode = writeFile) then 
    Rewrite(textfile);
  {$I+}
  CheckIOError('An oopsie happend!');
end;

procedure TransformText(var line: STRING);
begin
  line := UPCASE(line);
end;

function GetFilename(mode: FileMode): STRING;
var
  filename: STRING;
  cliIndex: INTEGER;
begin
  if(mode = readFile) then
    cliIndex := 1
  else if(mode = writeFile) then
    cliIndex := 2;

  if(cliIndex <= ParamCount) then
    filename := ParamStr(cliIndex)
  else begin
    write('enter ', mode, ' > ');
    ReadLn(filename);
  end;

  GetFilename := filename;
end;

var
  inputFile: TEXT;
  outputFile: TEXT;
  inFilename, outFilename, line: STRING;
begin
  inFilename := GetFilename(readFile);
  outFilename := GetFilename(writeFile);

  OpenTextFile(inputFile, inFilename, readFile);
  OpenTextFile(outputFile, outFilename, writeFile);

  while(not Eof(inputFile)) do
  begin
    ReadLn(inputFile, line);
    TransformText(line);
    writeln(outputFile, line);
  end;

  CloseTextFile(inputFile);
  CloseTextFile(outputFile); 
end.

(* BinFilter:                                         Wurm Elias, 2023-03-29 *)
(* ------                                                                    *)
(* Simple Text Filter Program to filter binfiles                             *)
(* ========================================================================= *)

program BinFilter;
const
  maxBufferSize = 10240;

type
  FileMode = (readFile, writeFile);
  BufferType = array[1..maxBufferSize] of CHAR;

procedure CheckIOError(msg: STRING);
var
  errorCode: INTEGER;
begin
  errorCode := IOResult;
  if(errorCode <> 0) then
    writeln('ERROR ', errorCode,' :', msg);
end;

procedure CloseBinFile(var f: file);
begin
  Close(f);
  CheckIOError('An other oopsie happend!');
end;

procedure OpenBinFile(var f: file; filename: STRING; mode: FileMode);
begin
  Assign(f, filename);
  {$I-}
  if(mode = readFile) then
    Reset(f, 1)
  else if(mode = writeFile) then 
    Rewrite(f, 1);
  {$I+}
  CheckIOError('An oopsie happend!');
end;

procedure TransformBuffer(var buf: BufferType; size: INTEGER);
var
  i: INTEGER;
begin
  for i := low(buf) to size do
    buf[i] := Upcase(buf[i]);
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
  inputFile: file;
  outputFile: file;
  inFilename, outFilename: STRING;
  buf: BufferType;
  bytesRead, bytesWritten: LONGINT;
begin
  inFilename := GetFilename(readFile);
  outFilename := GetFilename(writeFile);

  OpenBinFile(inputFile, inFilename, readFile);
  OpenBinFile(outputFile, outFilename, writeFile);

  BlockRead(inputFile, buf, maxBufferSize, bytesRead);
  while(bytesRead > 0) do
  begin
    TransformBuffer(buf, bytesRead);
    BlockWrite(outputFile, buf, bytesRead, bytesWritten);
    BlockRead(inputFile, buf, maxBufferSize, bytesRead);
  end;

  CloseBinFile(inputFile);
  CloseBinFile(outputFile); 
end.

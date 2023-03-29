(* RLE:                                               Wurm Elias, 2023-03-29 *)
(* ------                                                                    *)
(* run length encoding(RLE): is a method to compress text                    *)
(* ========================================================================= *)

program RLE;

uses SysUtils;

function CompressString(var str: string): string;
var
  i, j: integer;
  currChar: char;
  ret: string;

  procedure UpdateRet;
  begin
    if j > 2 then
      ret := ret + currChar + IntToStr(j)
    else if j = 2 then
      ret := ret + currChar + currChar
    else
      ret := ret + currChar;
    j := 1;
  end;
begin
  ret := '';
  j := 1;
  i := 1;
  currChar := str[i];
  for i := 2 to Length(str) do
    if str[i] = currChar then
      Inc(j)
    else
    begin
      UpdateRet();
      currChar := str[i];
    end;

  UpdateRet();
  CompressString := ret;
end;

function DecompressString(var str: STRING): STRING;
begin
  writeln(str);
  DecompressString := str;
end;

procedure CompressFile(const InFileName, OutFileName: string);
var
  line: STRING;
begin
  line := 'commmpresss';
  WriteLn(CompressString(line));
end;

procedure DecompressFile(InFileName, OutFileName: string);
var
  line: STRING;
begin
  line := 'decompress';
  DecompressString(line);
end;

var
  Command: string;
  InFileName, OutFileName: string;
begin
  if ParamCount > 0 then
    Command := ParamStr(1)
  else begin
    write('enter if you want to compress (-c) or decompress (-d) > ');
    ReadLn(Command);
  end;
  if (Command <> '-c') and (Command <> '-d') then
  begin
    WriteLn('Error: Unkowm Command - ', Command);
    writeln;
    Halt(1);
  end;

  if ParamCount > 1 then
    InFileName := ParamStr(2)
  else begin
    write('enter infilename > ');
    ReadLn(InFileName);
  end;
  if not FileExists(InFileName) then
  begin
    WriteLn('Error: input file does not exist - ', InFileName);
    writeln;
    Halt(1);
  end;

  if ParamCount > 2 then
    OutFileName := ParamStr(3)
  else begin
    write('enter outfilename > ');
    ReadLn(OutFileName);
  end;
  if (inFilename = outFilename) then
  begin
    WriteLn('Error: output file can not be the same as input file - ', InFileName);
    writeln;
    Halt(1);
  end;

  if Command = '-c' then
    CompressFile(InFileName, OutFileName)
  else
    DecompressFile(InFileName, OutFileName);
end.

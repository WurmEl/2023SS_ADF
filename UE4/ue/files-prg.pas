(* Files Program:                                     Wurm Elias, 2023-03-29 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)

program FilesProgram;
uses sysutils;

type
  MoodType = (happy, sad, angry, funny, bored);
  Person = record
    name: STRING[25];
    age: Integer;
    mood: MoodType;
  end;

var
  personfile: file of Person;
  // textfile: TEXT;
  // s: STRING;
  p: Person;
begin
  Assign(personfile, 'person.dat');
  // {$I-}
  // Rewrite(personfile);
  // {$I+}
  errorCode := IOResult;
  if(errorCode = 2) then writeln('file not found');
  
  // p.name := 'Hans';
  // p.age := 23;
  // p.mood := happy;
  // write(personfile, p);
  // Close(personfile);
  // Reset(personfile);
  // Read(personfile, p);
  // writeln(p.name);
  // writeln(p.age);
  // writeln(p.mood);
  close(personfile);
  // Assign(textfile, 'myfile.txt');
  // Rewrite(textfile);
  // writeln(textfile, 'Hello World!');
  // writeln(textfile, 'Hello World2!');
  // Close(textfile);
  // Reset(textfile);
  // while (not eof(textfile)) do
  // begin
  //   ReadLn(textfile, s);
  //   writeln(s);
  // end;
  // Close(textfile);
end.

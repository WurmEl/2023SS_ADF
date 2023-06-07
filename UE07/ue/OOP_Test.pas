(* OOP_Test:                                          Elias Wurm, 2023-05-10 *)
(* ------                                                                    *)
(* Test some OOP principles in pascal.                                       *)
(* ========================================================================= *)
program OOP_Test;

uses personUnit, studentUnit;

var
  s: Student;
  p: Person;
begin
  New(s, init('Jeff', 13, 2210));
  s^.PartyHard;
  writeln(s^.SayHello);
  p := s;
  writeln(p^.SayHello);
  Dispose(p, done);
end.

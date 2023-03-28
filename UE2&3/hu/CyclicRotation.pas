(* CyclicRotation:                                    Wurm Elias, 2023-03-28 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)

program CyclicRotation;

function IsCyclicRotation(s1, s2: STRING): BOOLEAN;
var
  i,j,k,s1Len, s2Len: INTEGER;
  ret: boolean;
begin
  s1Len := LENGTH(s1);
  s2Len := LENGTH(s2);

  ret := false;

  i := 0;
  if((s1Len <> 0) and (s2Len <> 0) and (s1Len = s2Len)) then
    while (i < s1Len) and (ret = false) do 
    begin
      j := 1;
      k := 1;
      while((s1[i+j] = s2[k]) and (k <= s1Len)) do
      begin
        Inc(j);
        Inc(k);
        if(((i + j) > s1Len)) then
          j := 1 - i// if out of bounds set to start
        ;
      end;
      if(k = s1Len+1) then
        ret := true;
      Inc(i);
    end;
  
  writeln(i);
  IsCyclicRotation := ret;
end;

begin
  writeln(IsCyclicRotation('abc','abc'));
  writeln(IsCyclicRotation('abcd','abce'));
  writeln(IsCyclicRotation('cab','abc'));
end.

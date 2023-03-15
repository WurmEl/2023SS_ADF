(* Pattern Matching:                                  Wurm Elias, 2023-03-15 *)
(* ------                                                                    *)
(* some pattern matching algorithms                                          *)
(* ========================================================================= *)
program PatternMatching;

type PatternMatcher = function(s,p:string):integer;

var
  charComparisons: Integer;

procedure ResetCharComparisons;
begin
  charComparisons := 0;
end;

procedure WriteCharComparisons;
begin
  writeln('Char Comparisons: ', charComparisons);
  ResetCharComparisons();
end;

function Equals(a,b:STRING): BOOLEAN;
begin
  Inc(charComparisons);
  Equals := a = b;
end;

function BruteSearchLR1(s,p:string):integer;
var
  i,j: INTEGER;
  sLen, pLen: INTEGER;
begin
  sLen := Length(s);
  pLen := Length(p);

  if ((sLen = 0) or (pLen = 0) or (sLen < pLen)) then BruteSearchLR1 := 0 else begin
    i := 1;
    repeat
      j := 1;
      while((j <= pLen) and Equals(s[i + j - 1], p[j])) do Inc(j);
      Inc(i);
    until ((j > pLen) or (i > (sLen - pLen + 1)));
  end;

  if (j > pLen) then BruteSearchLR1 := i - 1 else BruteSearchLR1 := 0;
end;

function BruteSearchLR2(s,p:string):integer;
var
  i,j: INTEGER;
  sLen, pLen: INTEGER;
begin
  sLen := Length(s);
  pLen := Length(p);

  if ((sLen = 0) or (pLen = 0) or (sLen < pLen)) then BruteSearchLR2 := 0 else begin
    i := 1;
    j := 1;
    repeat
      if(Equals(s[i], p[j])) then
      begin
        Inc(i);
        Inc(j);
      end else begin
        i := i - j + 2;
        j := 1;
      end;
    until ((j > pLen) or (i > sLen));
  end;

  if (j > pLen) then BruteSearchLR2 := i - pLen else BruteSearchLR2 := 0;
end;

function KMPSearch(s,p:string):integer;
var
  i,j: INTEGER;
  sLen, pLen: INTEGER;
  next: array[1..255] of integer;

  procedure InitNext;
  begin
    i := 1;
    j := 0;

    next[i] := 0;

    while(i < pLen) do
      if((j = 0) or Equals(p[i], p[j])) then
      begin
        Inc(i);
        Inc(j);
        next[i] := j;
      end else j := next[j];
  end;
begin
  sLen := Length(s);
  pLen := Length(p);
  
  InitNext;

  if ((sLen = 0) or (pLen = 0) or (sLen < pLen)) then KMPSearch := 0 else begin
    i := 1;
    j := 1;
    repeat
      if((j = 0) or Equals(s[i], p[j])) then
      begin
        Inc(i);
        Inc(j);
      end else j := next[j];
    until ((j > pLen) or (i > sLen));
  end;

  if (j > pLen) then KMPSearch := i - pLen else KMPSearch := 0;
end;

function KMPSearchOptimized(s,p:string):integer;
var
  i,j: INTEGER;
  sLen, pLen: INTEGER;
  next: array[1..255] of integer;

  procedure InitNext;
  begin
    i := 1;
    j := 0;

    next[i] := 0;

    while(i < pLen) do
      if((j = 0) or Equals(p[i], p[j])) then
      begin
        Inc(i);
        Inc(j);
        if (not Equals(p[i], p[j])) then next[i] := j else next[i] := next[j];
      end else j := next[j];
  end;
begin
  sLen := Length(s);
  pLen := Length(p);
  
  InitNext;

  if ((sLen = 0) or (pLen = 0) or (sLen < pLen)) then KMPSearchOptimized := 0 else begin
    i := 1;
    j := 1;
    repeat
      if((j = 0) or Equals(s[i], p[j])) then
      begin
        Inc(i);
        Inc(j);
      end else j := next[j];
    until ((j > pLen) or (i > sLen));
  end;

  if (j > pLen) then KMPSearchOptimized := i - pLen else KMPSearchOptimized := 0;
end;

procedure TestPatternMatcher(pm: PatternMatcher; pmName, s,p: STRING; expected: INTEGER);
begin
  if(pm(s,p) = expected) then WriteLn(pmName,' pass') else WriteLn(pmName,' failed expected: ', expected, ' got ', pm(s,p));
end;

procedure TestAllPatternMatcher(s,p: STRING; expected: INTEGER);
begin
  WriteLn('Testing: s=',s,', p=',p);
  TestPatternMatcher(BruteSearchLR1,'BruteSearchLR1',s,p,expected);
  WriteCharComparisons();
  TestPatternMatcher(BruteSearchLR2,'BruteSearchLR2',s,p,expected);
  WriteCharComparisons();
  TestPatternMatcher(KMPSearch,'KMPSearch',s,p,expected);
  WriteCharComparisons();
  TestPatternMatcher(KMPSearchOptimized,'KMPSearchOptimized',s,p,expected);
  WriteCharComparisons();
  WriteLn;
end;

begin
  TestAllPatternMatcher('Hagenberg', 'Hag', 1);
  TestAllPatternMatcher('Hagenberg', 'enb', 4);
  TestAllPatternMatcher('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab', 'aaaaaaaab', 41);
  TestAllPatternMatcher('abababababababababababababababac', 'abac', 29 );
end.

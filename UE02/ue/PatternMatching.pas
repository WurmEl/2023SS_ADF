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

function BruteSearchRL(s,p:string):integer;
var
  i,j: INTEGER;
  sLen, pLen: INTEGER;
begin
  sLen := Length(s);
  pLen := Length(p);

  if ((sLen = 0) or (pLen = 0) or (sLen < pLen)) then BruteSearchRL := 0 else begin
    i := pLen;
    j := pLen;
    repeat
      if(Equals(s[i], p[j])) then
      begin
        Dec(i);
        Dec(j);
      end else begin
        i := i + pLen - j + 1;
        j := pLen;
      end;
    until ((j < 1) or (i > sLen));
  end;

  if (j < 1) then BruteSearchRL := i + 1 else BruteSearchRL := 0;
end;

function BoyerMooreSearch(s,p:string):integer;
var
  i,j: INTEGER;
  sLen, pLen: INTEGER;
  skip: array[char] of INTEGER;

  procedure InitSkip;
  var i: INTEGER;
  begin
    for i := 0 to 255 do
      skip[Char(i)] := pLen;
    for i := 1 to pLen do
      skip[p[i]] := pLen - i;
  end;

begin
  sLen := Length(s);
  pLen := Length(p);

  if ((sLen = 0) or (pLen = 0) or (sLen < pLen)) then BoyerMooreSearch := 0 else begin
    InitSkip;
    i := pLen;
    j := pLen;
    repeat
      if(Equals(s[i], p[j])) then
      begin
        Dec(i);
        Dec(j);
      end else begin
        if(skip[s[i]] < pLen - j + 1) then i := i + pLen - j + 1 else i := i + skip[s[i]];
        j := pLen;
      end;
    until ((j < 1) or (i > sLen));
  end;

  if (j < 1) then BoyerMooreSearch := i + 1 else BoyerMooreSearch := 0;
end;

function RabinKarpSearch(s,p:string):integer;
const 
  basis = 256;
  m = 8355967;

var
  i, j, k: INTEGER;
  sLen, pLen, bPLen: INTEGER;
  sHash, pHash: LONGINT;
begin
  sLen := Length(s);
  pLen := Length(p);

  if ((sLen = 0) or (pLen = 0) or (sLen < pLen)) then RabinKarpSearch := 0 else begin
    pHash := 0;
    sHash := 0;
    for i := 1 to pLen do
    begin
      pHash := (pHash * basis + Ord(p[i])) mod m;
      sHash := (sHash * basis + Ord(s[i])) mod m;
    end;

    bPLen := 0;
    for i := 1 to pLen do 
      bPLen := (bPLen + basis) mod m;

    i := 1;
    j := 1;
    while((i <= sLen - pLen + 1) and (j <= pLen)) do
    begin
      if(pHash = sHash) then
      begin
        j := 1;
        k := i;

        while((j <= pLen) and Equals(s[k], p[j])) do
        begin
          Inc(k);
          Inc(j);
        end;
      end;
      sHash := (basis * m + sHash - Ord(s[i]) * bPLen) mod m;
      if(i < sLen) then sHash := (sHash * basis + Ord(s[i + pLen])) mod m;
      Inc(i);
    end;

    if(j > pLen) then RabinKarpSearch := i - 1 else RabinKarpSearch := 0;
  end;
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
  TestPatternMatcher(BruteSearchRL,'BruteSearchRL',s,p,expected);
  WriteCharComparisons();
  TestPatternMatcher(BoyerMooreSearch,'BoyerMooreSearch',s,p,expected);
  WriteCharComparisons();
  TestPatternMatcher(RabinKarpSearch,'RabinKarpSearch',s,p,expected);
  WriteCharComparisons();
  WriteLn;
end;

begin
  TestAllPatternMatcher('Hagenberg', 'Hag', 1);
  TestAllPatternMatcher('Hagenberg', 'enb', 4);
  TestAllPatternMatcher('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab', 'aaaaaaaab', 41);
  TestAllPatternMatcher('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaa', 'baaaaaaaa', 41);
  TestAllPatternMatcher('abababababababababababababababac', 'abac', 29 );
  TestAllPatternMatcher('Betreft zij heb hiertoe bijgang evenals. Mislukking instorting om voorschijn gomsoorten er denzelfden. Ook onder geest tabak ten weten zee. Schuld nu langen liever alleen al buizen af werden. En kwam wild toch af in bouw.', 'fleischi', 0 );
  TestAllPatternMatcher('01011101001101100000000100000010000011101011001101001101001011111101101110111110001001110111100110011001111001100001100010010101101001001110010101100100110001101110111010011000000100010111110101111111', '1010111010', 0 );
end.

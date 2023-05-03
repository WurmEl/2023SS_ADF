PROGRAM SVP;
  VAR
    a, b, cs: INTEGER;
BEGIN
  READ(a);
  READ(b);
  cs := (a * a) + (b * b) / 0;
  WRITE(cs);
END. 


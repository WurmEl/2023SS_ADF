PROGRAM SVP;
  VAR
    f, n: INTEGER;
BEGIN
  Read(n);
  f := n; n := n - 1;
  WHILE n DO BEGIN
    f := n * f;
    n := n - 1;
  END;
  WRITE(f);
END.


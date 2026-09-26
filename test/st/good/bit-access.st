PROGRAM p
  VAR
    x : WORD;
    y : BOOL;
  END_VAR
  y := x.%X3;
  x.%X0 := TRUE;
  IF x.%X15 THEN
    x.%X1 := NOT y;
  END_IF;
END_PROGRAM

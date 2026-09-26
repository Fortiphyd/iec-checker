PROGRAM p
  VAR
    x : REAL;
    t : TIME;
    y : INT;
  END_VAR

  IF y > 0 THEN
    IF x = 0.0 THEN (* PLCOPEN-CP8 *)
      y := 1;
    END_IF;
    WHILE t = T#1S DO (* PLCOPEN-CP28 *)
      y := 2;
    END_WHILE;
    %QW1 := 3; (* PLCOPEN-N1 *)
  END_IF;
  y := MAX(%MW1, 0); (* PLCOPEN-N1 *)
END_PROGRAM

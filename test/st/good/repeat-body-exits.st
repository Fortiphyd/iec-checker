(* REPEAT bodies whose last statement has several exits. *)
PROGRAM p
  VAR
    a, i : INT;
    c : BOOL;
  END_VAR
  REPEAT
    IF c THEN
      a := 1;
    ELSE
      a := 2;
    END_IF;
  UNTIL c END_REPEAT;

  REPEAT
    CASE a OF
      1: a := 2;
      2: a := 3;
    END_CASE;
  UNTIL c END_REPEAT;

  REPEAT
    a := a + 1;
    IF c THEN
      EXIT;
    END_IF;
  UNTIL a > 5 END_REPEAT;

  REPEAT
    REPEAT
      IF c THEN
        a := 1;
      END_IF;
    UNTIL c END_REPEAT;
  UNTIL c END_REPEAT;

  a := 99;
END_PROGRAM

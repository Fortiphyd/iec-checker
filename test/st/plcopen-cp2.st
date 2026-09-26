FUNCTION f1 : INT
  VAR i : INT; END_VAR
  RETURN;
  WHILE i < 10 DO (* CP2 *)
    i := i + 1;
  END_WHILE;
END_FUNCTION

FUNCTION f2 : INT
  VAR c : BOOL; i : INT; END_VAR
  IF c THEN
    RETURN;
  ELSE
    RETURN;
  END_IF;
  i := 1; (* CP2 *)
END_FUNCTION

PROGRAM p
  VAR c : BOOL; i : INT; END_VAR
  IF FALSE THEN (* bypass idiom of the rule *)
    i := 1;
  END_IF;
  IF 1 > 2 THEN
    i := 2; (* CP2 *)
  END_IF;
  WHILE FALSE DO
    i := 3; (* CP2 *)
  END_WHILE;
  IF TRUE THEN
    i := 4;
  ELSIF c THEN
    i := 5; (* CP2 *)
  ELSE
    i := 6; (* CP2 *)
  END_IF;
  FOR i := 5 TO 1 DO
    c := TRUE; (* CP2 *)
  END_FOR;
  FOR i := 5 TO 1 BY -1 DO
    c := TRUE;
  END_FOR;
  IF c THEN
    RETURN;
  END_IF;
  i := 7; (* reachable when c is FALSE *)
  REPEAT
    i := i + 1;
    EXIT;
  UNTIL c END_REPEAT;
  i := 8; (* reachable after EXIT *)
END_PROGRAM

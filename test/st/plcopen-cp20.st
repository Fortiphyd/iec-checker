FUNCTION_BLOCK counter
  VAR_INPUT
    x : BOOL;
  END_VAR
  VAR_OUTPUT
    n : INT;
  END_VAR
  IF x THEN
    n := n + 1;
  END_IF;
END_FUNCTION_BLOCK

FUNCTION helper : INT
  VAR_INPUT
    a : INT;
  END_VAR
  helper := a;
END_FUNCTION

PROGRAM demo
  VAR
    edge1, edge2 : R_TRIG;
    t1 : TON;
    c1 : counter;
    input_a, input_b, run : BOOL;
    i, y : INT;
  END_VAR
  edge1(CLK := input_a);
  edge1(CLK := input_b); (* PLCOPEN-CP20 *)
  edge2(CLK := input_a); (* a different instance *)
  IF run THEN (* exclusive branches are fine *)
    t1(IN := TRUE, PT := T#1S);
  ELSE
    t1(IN := FALSE, PT := T#1S);
  END_IF;
  FOR i := 0 TO 3 DO
    c1(x := run); (* PLCOPEN-CP20 *)
  END_FOR;
  helper(a := 1); (* functions can be called any number of times *)
  helper(a := 2);
  y := helper(3);
END_PROGRAM

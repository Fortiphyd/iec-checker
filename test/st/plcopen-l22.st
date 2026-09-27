FUNCTION_BLOCK Stepper
  VAR_INPUT step : INT; END_VAR
  VAR_IN_OUT pos : INT; END_VAR
  VAR_OUTPUT next : INT; END_VAR
  pos := pos + step;
  next := pos;
END_FUNCTION_BLOCK

FUNCTION Bump : INT
  VAR_IN_OUT x : INT; END_VAR
  x := x + 1;
  Bump := x;
END_FUNCTION

PROGRAM l22
  VAR
    i : INT := 0;
    j : INT := 0;
    n : INT := 10;
    k : INT := 1;
    arr : ARRAY[0..10] OF INT;
    cfg : Limits;
    st : Stepper;
  END_VAR

  (* Case 1: Direct assignment in body — violation *)
  FOR i := 0 TO 10 BY 2 DO
    IF arr[i] = 42 THEN
      i := 10; (* L22 *)
    END_IF;
  END_FOR;

  (* Case 2: No violation — different variable *)
  FOR i := 0 TO 10 DO
    j := i + 1;
  END_FOR;

  (* Case 3: Nested FOR — inner loop modifies outer var — violation *)
  FOR i := 0 TO 10 DO
    FOR j := 0 TO 5 DO
      i := j; (* L22 *)
    END_FOR;
  END_FOR;

  (* Case 4: Nested FOR — inner loop modifies its own var — violation *)
  FOR i := 0 TO 10 DO
    FOR j := 0 TO 5 DO
      j := 0; (* L22 *)
    END_FOR;
  END_FOR;

  (* Case 5: Nested FOR reuses the outer control variable *)
  FOR i := 0 TO 10 DO
    FOR i := 0 TO 5 DO (* L22 *)
      j := i;
    END_FOR;
  END_FOR;

  (* Case 6: Written through an output and a VAR_IN_OUT *)
  FOR i := 0 TO 10 DO
    st(step := 1, pos := i, next => j); (* L22 *)
  END_FOR;
  FOR j := 0 TO 10 DO
    st(step := 1, pos := k, next => j); (* L22 *)
  END_FOR;
  FOR i := 0 TO 10 DO
    j := Bump(i); (* L22 *)
  END_FOR;

  (* Case 7: The final value and increment change *)
  FOR i := 0 TO n BY k DO
    n := n - 1; (* L22 *)
    k := 2; (* L22 *)
  END_FOR;
  FOR i := 0 TO cfg.max DO
    cfg.max := 5; (* L22 *)
    cfg.min := 1;
  END_FOR;
  FOR i := 0 TO arr[j] DO
    j := j + 1; (* L22 *)
  END_FOR;

  (* Case 8: The initial value is evaluated once *)
  FOR i := j TO 10 DO
    j := 0;
  END_FOR;
END_PROGRAM

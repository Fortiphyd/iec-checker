FUNCTION fa : INT
  VAR_INPUT x : INT; END_VAR
  fa := fb_(x - 1); (* CP13 *)
END_FUNCTION
FUNCTION fb_ : INT
  VAR_INPUT x : INT; END_VAR
  fb_ := fa(x); (* CP13 *)
END_FUNCTION
FUNCTION fact : INT
  VAR_INPUT x : INT; END_VAR
  fact := MAX(IN1 := fact(x - 1), IN2 := 1); (* CP13 *)
END_FUNCTION
FUNCTION fact3 : INT
  VAR_INPUT x : INT; END_VAR
  fact3 := ABS(fact3(x - 1)); (* CP13 *)
END_FUNCTION
FUNCTION_BLOCK loop_fb
  VAR inner : loop_fb2; END_VAR
  inner(); (* CP13 *)
END_FUNCTION_BLOCK
FUNCTION_BLOCK loop_fb2
  VAR back : loop_fb; END_VAR
  back(); (* CP13 *)
END_FUNCTION_BLOCK
FUNCTION helper : INT
  helper := 1;
END_FUNCTION
FUNCTION user : INT
  user := helper() + helper();
END_FUNCTION

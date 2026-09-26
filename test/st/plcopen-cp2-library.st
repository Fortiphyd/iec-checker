(* A library: no program uses these here, but applications do. *)
FUNCTION Scale : INT
  VAR_INPUT x : INT; END_VAR
  Scale := x * 2;
END_FUNCTION

FUNCTION_BLOCK Valve
  VAR_OUTPUT open : BOOL; END_VAR
  open := TRUE;
END_FUNCTION_BLOCK

PROGRAM prog0   (* PLCOPEN-N9 *)
  VAR
    local0 : INT;
    fast : INT; (* tasks aren't in the scope of POUs *)
  END_VAR
  local0 := 0;
END_PROGRAM

FUNCTION_BLOCK MyCalculation   (* PLCOPEN-N9 *)
  VAR_IN_OUT
    MyCalculation : REAL;   (* PLCOPEN-N9 *)
  END_VAR
  MyCalculation := 1.1;
END_FUNCTION_BLOCK

(* Different kinds of POUs *)
FUNCTION Scale : INT   (* PLCOPEN-N9 *)
  VAR_INPUT x : INT; END_VAR
  Scale := x;
END_FUNCTION

FUNCTION_BLOCK Scale   (* PLCOPEN-N9 *)
  VAR_OUTPUT y : INT; END_VAR
  y := 0;
END_FUNCTION_BLOCK

(* Variables of different POUs don't share a scope *)
FUNCTION_BLOCK Other   (* PLCOPEN-N9 *)
  VAR local0 : INT; END_VAR
  local0 := 1;
END_FUNCTION_BLOCK

CONFIGURATION config
  VAR_GLOBAL
    MyCalculation : REAL;   (* PLCOPEN-N9 *)
    slow : INT;   (* PLCOPEN-N9 *)
  END_VAR
  RESOURCE res ON PLC
    TASK task0(INTERVAL := T#20ms, PRIORITY := 0);
    TASK fast(INTERVAL := T#10ms, PRIORITY := 1);
    TASK slow(INTERVAL := T#100ms, PRIORITY := 2);   (* PLCOPEN-N9 *)
    TASK prog0(INTERVAL := T#50ms, PRIORITY := 3);   (* PLCOPEN-N9 *)
    PROGRAM instance0 WITH task0 : prog0;
    PROGRAM Other WITH fast : prog0;   (* PLCOPEN-N9 *)
  END_RESOURCE
END_CONFIGURATION

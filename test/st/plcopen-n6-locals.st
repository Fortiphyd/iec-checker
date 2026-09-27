FUNCTION_BLOCK FeedControl
  VAR_INPUT
    enable : BOOL;   (* PLCOPEN-N6 *)
    startRequest : BOOL;
  END_VAR
  VAR
    i : INT;
    j : INT;   (* PLCOPEN-N6 *)
    sum : INT;
    MaximumTemperatureForTheThermocoupleInput : REAL;   (* PLCOPEN-N6 *)
  END_VAR
  FOR i := 1 TO 10 DO
    sum := sum + i;
  END_FOR;
  j := sum;
END_FUNCTION_BLOCK

FUNCTION Go : BOOL   (* PLCOPEN-N6 *)
  Go := TRUE;
END_FUNCTION

PROGRAM MainProgram
  VAR ctl : FeedControl; END_VAR
  ctl(enable := TRUE);
END_PROGRAM

CONFIGURATION PlantConfig
  VAR_GLOBAL
    speed : INT;   (* PLCOPEN-N6 *)
    conveyorSpeed : INT;
  END_VAR
  RESOURCE res ON PLC
    VAR_GLOBAL
      level : INT;   (* PLCOPEN-N6 *)
    END_VAR
    TASK fast(INTERVAL := T#10ms, PRIORITY := 1);   (* PLCOPEN-N6 *)
    TASK SlowCycle(INTERVAL := T#100ms, PRIORITY := 2);
    PROGRAM mainInst WITH SlowCycle : MainProgram;
  END_RESOURCE
END_CONFIGURATION

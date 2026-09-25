CONFIGURATION cfg
  VAR_GLOBAL
    g_valve AT %QX1.0 : BOOL;
  END_VAR
  RESOURCE res ON PLC
    TASK main(INTERVAL := T#10MS, PRIORITY := 1);
    PROGRAM inst WITH main : demo;
  END_RESOURCE
END_CONFIGURATION

FUNCTION_BLOCK act
  VAR_OUTPUT
    out : INT;
  END_VAR
  out := 1;
END_FUNCTION_BLOCK

PROGRAM demo
  VAR_EXTERNAL
    g_valve : BOOL;
  END_VAR
  VAR
    motor AT %QW0 : INT;
    lamps AT %QW10 : WORD;
    temp : INT;
    alarm, run : BOOL;
    i : INT;
    a1 : act;
  END_VAR
  %QW1 := temp;
  IF alarm THEN
    %QW1 := 0; (* PLCOPEN-CP12 *)
  END_IF;
  IF run THEN (* exclusive branches are fine *)
    %QW3 := 1;
  ELSE
    %QW3 := 0;
  END_IF;
  motor := temp;
  %QW0 := 5; (* PLCOPEN-CP12 *)
  lamps.%X0 := TRUE; (* different bits are different outputs *)
  lamps.%X1 := TRUE;
  lamps.%X1 := FALSE; (* PLCOPEN-CP12 *)
  FOR i := 0 TO 3 DO
    %QW4 := i; (* PLCOPEN-CP12 *)
  END_FOR;
  g_valve := run;
  g_valve := alarm; (* PLCOPEN-CP12 *)
  a1(out => %QW5);
  %QW5 := 2; (* PLCOPEN-CP12 *)
  IF run THEN
    %QW6 := 1;
  ELSE
    %QW6 := 0;
  END_IF;
  %QW6 := 2; (* PLCOPEN-CP12 *)
  IF alarm THEN
    %QW7 := 0;
    RETURN;
  END_IF;
  %QW7 := 1; (* not in the same cycle as the write before RETURN *)
  temp := 1; (* not an output *)
  temp := 2;
END_PROGRAM

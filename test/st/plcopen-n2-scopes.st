TYPE Point : STRUCT x : INT; y : INT; END_STRUCT; END_TYPE
TYPE Mode : (Auto, Manual); END_TYPE
TYPE Temperature : REAL; END_TYPE
TYPE Counter : INT; END_TYPE

FUNCTION_BLOCK Pump
  VAR_INPUT
    pxEnable : BOOL;
    xEnable : BOOL;         (* PLCOPEN-N2 *)
  END_VAR
  VAR_OUTPUT
    piCount : INT;
  END_VAR
  VAR
    xReady : BOOL;
    x_done : BOOL;
    xylophone : BOOL;       (* PLCOPEN-N2 *)
    index : INT;            (* PLCOPEN-N2 *)
    todStart : TIME_OF_DAY;
    todEnd : TOD;
    StartAt : TOD;          (* PLCOPEN-N2 *)
    aValues : ARRAY [0..3] OF INT;
    values : ARRAY [0..3] OF INT;  (* PLCOPEN-N2 *)
    stOrigin : Point;
    origin : Point;         (* PLCOPEN-N2 *)
    eMode : Mode;
    mode : Mode;            (* PLCOPEN-N2 *)
    fbTimer : TON;
    timer : TON;            (* PLCOPEN-N2 *)
    tempOven : Temperature;
    oven : Temperature;     (* PLCOPEN-N2 *)
    iTotal : Counter;
    total : Counter;        (* PLCOPEN-N2 *)
    unknown : SomeType;
  END_VAR
  xReady := pxEnable;
END_FUNCTION_BLOCK

PROGRAM Main
  VAR xOn : BOOL; END_VAR
  xOn := TRUE;
END_PROGRAM

CONFIGURATION cfg
  VAR_GLOBAL
    gxRunning : BOOL;
    xStopped : BOOL;        (* PLCOPEN-N2 *)
    gCount : INT;           (* PLCOPEN-N2 *)
  END_VAR
  RESOURCE res ON PLC
    VAR_GLOBAL
      giLevel : INT;
      level : INT;          (* PLCOPEN-N2 *)
    END_VAR
    TASK main(INTERVAL := T#10ms, PRIORITY := 1);
    PROGRAM inst WITH main : Main;
  END_RESOURCE
END_CONFIGURATION

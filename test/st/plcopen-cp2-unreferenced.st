CONFIGURATION cfg
  VAR_GLOBAL
    g_valve : Valve;
  END_VAR
  RESOURCE res ON PLC
    TASK t (INTERVAL := T#10ms, PRIORITY := 1);
    PROGRAM main_inst WITH t : Main;
  END_RESOURCE
END_CONFIGURATION

FUNCTION Scale : INT
  VAR_INPUT x : INT; END_VAR
  Scale := x * 2;
END_FUNCTION

FUNCTION Clamp : INT
  VAR_INPUT x : INT; END_VAR
  Clamp := x;
END_FUNCTION

FUNCTION Unused : INT (* CP2 *)
  VAR_INPUT x : INT; END_VAR
  Unused := x;
END_FUNCTION

FUNCTION Recurse : INT (* CP2 *)
  VAR_INPUT n : INT; END_VAR
  Recurse := Recurse(n - 1);
END_FUNCTION

FUNCTION OnlyForDeadFb : INT (* CP2 *)
  OnlyForDeadFb := 1;
END_FUNCTION

FUNCTION_BLOCK Valve
  VAR_OUTPUT open : BOOL; END_VAR
  open := TRUE;
END_FUNCTION_BLOCK

FUNCTION_BLOCK Pump
  VAR_OUTPUT running : BOOL; END_VAR
  running := TRUE;
END_FUNCTION_BLOCK

FUNCTION_BLOCK Mixer
  VAR_OUTPUT active : BOOL; END_VAR
  active := TRUE;
END_FUNCTION_BLOCK

FUNCTION_BLOCK DeadFb (* CP2 *)
  VAR_OUTPUT v : INT; END_VAR
  v := OnlyForDeadFb();
END_FUNCTION_BLOCK

TYPE Station :
  STRUCT
    mixer : Mixer;
  END_STRUCT;
END_TYPE

PROGRAM Main
  VAR
    pumps : ARRAY [1..2] OF Pump;
    st : Station;
    y : INT;
  END_VAR
  y := Clamp(Scale(y));
END_PROGRAM

PROGRAM Spare (* CP2 *)
  VAR y : INT; END_VAR
  y := 1;
END_PROGRAM

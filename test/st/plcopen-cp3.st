TYPE TempLimit : REAL := 250.0; END_TYPE
TYPE Plain : STRUCT a : INT; END_STRUCT END_TYPE

FUNCTION f : INT
  VAR_INPUT
    a : INT; (* set by the caller *)
  END_VAR
  VAR
    b : INT;
  END_VAR
  f := a + b; (* PLCOPEN-CP3 *)
END_FUNCTION

PROGRAM Initialization
  VAR
    NumOfRetries : INT := 3;
    StateLastPosition : INT;
    SpeedAverage : REAL;
    Boiler : TempLimit;
    Pipe : Plain;
    Counter : INT;
    Flag : BOOL;
    Both : INT;
    arr : ARRAY [0..3] OF INT;
    i : INT;
    x : INT;
    r : REAL;
    t : TON;
  END_VAR
  VAR RETAIN
    Kept : INT;
  END_VAR
  VAR
    start AT %IX0.0 : BOOL;
  END_VAR
  (* The rule's example: assigned before use *)
  StateLastPosition := 50;
  SpeedAverage := 0.0;
  x := StateLastPosition + NumOfRetries;
  x := Kept; (* RETAIN *)
  IF start THEN (* physical input *)
    Flag := TRUE;
  END_IF;
  x := Counter; (* PLCOPEN-CP3 *)
  Counter := Counter + 1;
  IF Flag THEN (* PLCOPEN-CP3 *)
    Both := 1;
  ELSE
    Both := 2;
  END_IF;
  x := Both; (* assigned in every branch *)
  FOR i := 0 TO 3 DO
    arr[i] := 0;
  END_FOR;
  x := arr[1]; (* the loop runs at least once *)
  x := Pipe.a; (* PLCOPEN-CP3 *)
  t(IN := Flag, PT := T#1S); (* function block instance *)
  r := Boiler; (* type with a default initial value *)
END_PROGRAM

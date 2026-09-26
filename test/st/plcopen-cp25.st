TYPE percent : USINT; END_TYPE

TYPE motor_t :
  STRUCT
    speed : DINT;
    temp : REAL;
  END_STRUCT
END_TYPE

FUNCTION_BLOCK sensor
  VAR_INPUT
    limit : INT;
  END_VAR
  VAR_OUTPUT
    value : LREAL;
  END_VAR
  value := 1.0;
END_FUNCTION_BLOCK

FUNCTION scale : DINT
  VAR_INPUT
    x : INT;
    k : REAL;
  END_VAR
  scale := x;
END_FUNCTION

PROGRAM demo
  VAR
    s : SINT;
    I : INT := 10;
    J : REAL := 0.55;
    d : DINT;
    u : UINT;
    lr : LREAL;
    p : percent;
    m : motor_t;
    arr : ARRAY [0..3] OF DINT;
    fb : sensor;
    c : BOOL;
  END_VAR
  (* Conversions without loss are allowed (Table 11) *)
  I := s;
  d := I;
  J := I;
  lr := J;
  c := I < J;
  (* Explicit conversions *)
  I := REAL_TO_INT(J);
  J := INT_TO_REAL(I);
  I := DINT_TO_INT(d);
  (* Lossy implicit conversions *)
  I := J; (* PLCOPEN-CP25 *)
  I := I * J; (* PLCOPEN-CP25 *)
  I := d; (* PLCOPEN-CP25 *)
  I := d + 1; (* PLCOPEN-CP25 *)
  u := I; (* PLCOPEN-CP25 *)
  J := lr; (* PLCOPEN-CP25 *)
  I := 5.7; (* PLCOPEN-CP25 *)
  s := 300; (* PLCOPEN-CP25 *)
  s := 100;
  u := -1; (* PLCOPEN-CP25 *)
  p := I; (* PLCOPEN-CP25 *)
  I := m.speed; (* PLCOPEN-CP25 *)
  I := arr[1]; (* PLCOPEN-CP25 *)
  J := fb.value; (* PLCOPEN-CP25 *)
  I := scale(I, J); (* PLCOPEN-CP25 *)
  I := %IW0; (* PLCOPEN-CP25 *)
  d := %IW0;
  (* Operands without a lossless common type *)
  c := d < J; (* PLCOPEN-CP25 *)
  c := I = u; (* PLCOPEN-CP25 *)
  (* Arguments *)
  fb(limit := d); (* PLCOPEN-CP25 *)
  fb(limit := I);
  d := scale(d, J); (* PLCOPEN-CP25 *)
  d := scale(x := I, k := lr); (* PLCOPEN-CP25 *)
END_PROGRAM

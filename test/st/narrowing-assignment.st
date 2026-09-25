TYPE percent : USINT; END_TYPE

TYPE motor_t :
  STRUCT
    speed : DINT;
    temp : REAL;
  END_STRUCT
END_TYPE

FUNCTION_BLOCK sensor
  VAR_OUTPUT
    value : LREAL;
  END_VAR
  value := 1.0;
END_FUNCTION_BLOCK

FUNCTION read_count : DINT
  read_count := 1;
END_FUNCTION

PROGRAM demo
  VAR
    s : SINT;
    i : INT;
    d : DINT;
    u : UINT;
    r : REAL;
    lr : LREAL;
    p : percent;
    m : motor_t;
    arr : ARRAY [0..3] OF DINT;
    fb : sensor;
  END_VAR
  i := s; (* widening is fine *)
  d := i;
  r := i;
  lr := r;
  i := d; (* NarrowingAssignment *)
  i := d + 1; (* NarrowingAssignment *)
  u := i; (* NarrowingAssignment *)
  r := lr; (* NarrowingAssignment *)
  i := r * 2.0; (* NarrowingAssignment *)
  i := r; (* reported by PLCOPEN-CP25 *)
  s := 300; (* NarrowingAssignment *)
  s := 100;
  u := -1; (* NarrowingAssignment *)
  i := DINT_TO_INT(d); (* explicit conversion *)
  p := i; (* NarrowingAssignment *)
  i := m.speed; (* NarrowingAssignment *)
  i := arr[1]; (* NarrowingAssignment *)
  r := fb.value; (* NarrowingAssignment *)
  i := read_count(); (* NarrowingAssignment *)
  i := %IW0; (* NarrowingAssignment *)
  d := %IW0;
END_PROGRAM

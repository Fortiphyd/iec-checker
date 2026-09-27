(* Programs of one task sharing outputs, globals and FB instances, directly
   and through a function block. See test_cross_pou in test_plcopen.py. *)
CONFIGURATION cfg
  VAR_GLOBAL
    gOut AT %QX2.0 : BOOL;
    gT : TON;
    g2 : INT;
    g3 : pt;
    g4 : BOOL;
    head AT %MW600 : INT;
    gm AT %MW451 : INT;
  END_VAR
  RESOURCE res ON PLC
    TASK fast(INTERVAL := T#10MS, PRIORITY := 1);
    PROGRAM i1 WITH fast : p1;
    PROGRAM i2 WITH fast : p2;
  END_RESOURCE
END_CONFIGURATION

TYPE pt : STRUCT x : INT; END_STRUCT END_TYPE

FUNCTION_BLOCK drv
  VAR_EXTERNAL gOut : BOOL; g2 : INT; gT : TON; END_VAR
  VAR_INPUT v : BOOL; END_VAR
  gOut := v;
  g2 := 1;
  gT(IN := v, PT := T#1S);
END_FUNCTION_BLOCK

PROGRAM p1
  VAR_EXTERNAL gOut : BOOL; gT : TON; g2 : INT; g3 : pt; g4 : BOOL; head : INT; END_VAR
  VAR
    out1 AT %QX0.0 : BOOL;
    r AT %MW450 : REAL;
    b1 AT %IX0.0 : BOOL;
    c : BOOL;
    t : TON;
    tmp : INT;
    d1 : drv;
    cnt : CTU;
    i : INT;
  END_VAR
  out1 := TRUE;
  gOut := c;
  gT(IN := c, PT := T#1S);
  g2 := 2;
  g3.x := 1;
  t(IN := c, PT := T#1S, Q => g4);
  %MW600 := 1;
  tmp := %MW600;
  d1(v := c);
  FOR i := 0 TO 3 DO
    cnt(CU := c);
  END_FOR;
END_PROGRAM

PROGRAM p2
  VAR_EXTERNAL gT : TON; g3 : pt; g4 : BOOL; END_VAR
  VAR
    out2 AT %QX0.0 : BOOL;
    r2 AT %MW450 : INT;
    b2 AT %IX0.0 : BOOL;
    c : BOOL;
    t : TON;
    d2 : drv;
  END_VAR
  out2 := TRUE;
  gT(IN := c, PT := T#1S);
  g3.x := 2;
  t(IN := c, PT := T#1S, Q => g4);
  d2(v := NOT c);
END_PROGRAM

TYPE motor_t :
  STRUCT
    temp : REAL;
  END_STRUCT
END_TYPE

FUNCTION avg : REAL
  avg := 1.0;
END_FUNCTION

PROGRAM demo
    VAR
        x : REAL;
        y : REAL;
        lr : LREAL;
        i, j : INT;
        ready, b : BOOL;
        m : motor_t;
    END_VAR

    IF x = 0.0 THEN (* PLCOPEN CP-8 *)
      x := x + 1;
    END_IF;

    IF x <> 0.0 THEN (* PLCOPEN CP-8 *)
      x := x + 1;
    END_IF;

    IF 0.0 = x THEN (* PLCOPEN CP-8 *)
      x := x + 1;
    END_IF;

    IF 0.0 <> x THEN (* PLCOPEN CP-8 *)
      x := x + 1;
    END_IF;

    IF REAL_TO_INT(x) <> 0 THEN
      x := x + 1;
    END_IF;

    IF x = y THEN (* PLCOPEN CP-8 *)
      x := 0.0;
    END_IF;
    IF lr <> x THEN (* PLCOPEN CP-8 *)
      x := 0.0;
    END_IF;
    IF x = 0 THEN (* PLCOPEN CP-8 *)
      x := 0.0;
    END_IF;
    IF ready AND x = y THEN (* PLCOPEN CP-8 *)
      x := 0.0;
    END_IF;
    IF m.temp = x THEN (* PLCOPEN CP-8 *)
      x := 0.0;
    END_IF;
    IF avg() <> x THEN (* PLCOPEN CP-8 *)
      x := 0.0;
    END_IF;
    b := x = y; (* PLCOPEN CP-8 *)
    IF i = j THEN (* integers can be compared for equality *)
      x := 0.0;
    END_IF;
    IF x > y THEN (* ordering comparisons are fine *)
      x := 0.0;
    END_IF;
END_PROGRAM

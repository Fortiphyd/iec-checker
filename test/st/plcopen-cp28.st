PROGRAM demo
    VAR
        x : TIME;
        y : TIME;
        t1 : TON;
        now, shift_start : TOD;
        today, due : DATE;
        ready, b : BOOL;
    END_VAR

    IF x = T#100MS THEN (* PLCOPEN CP-28 *)
      x := x + 1;
    END_IF;

    IF x <> T#100MS THEN (* PLCOPEN CP-28 *)
      x := x + 1;
    END_IF;

    IF T#100MS = x THEN (* PLCOPEN CP-28 *)
      x := x + 1;
    END_IF;

    IF T#100MS <> x THEN (* PLCOPEN CP-28 *)
      x := x + 1;
    END_IF;

    IF REAL_TO_INT(x) <> 0 THEN
      x := x + 1;
    END_IF;

    IF x = y THEN (* PLCOPEN CP-28 *)
      x := T#0S;
    END_IF;
    IF t1.ET = x THEN (* PLCOPEN CP-28 *)
      x := T#0S;
    END_IF;
    b := ready AND x <> y; (* PLCOPEN CP-28 *)
    IF now = shift_start THEN (* PLCOPEN CP-28 *)
      x := T#0S;
    END_IF;
    IF today = due THEN (* dates can be compared for equality *)
      x := T#0S;
    END_IF;
    IF t1.ET >= x THEN (* ordering comparisons are fine *)
      x := T#0S;
    END_IF;
END_PROGRAM


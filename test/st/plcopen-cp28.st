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

    IF REAL_TO_INT(x) <> 0 THEN (* PLCOPEN CP-28: a time in an integer *)
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
    IF today = D#2024-01-01 THEN (* date literals are dates *)
      x := T#0S;
    END_IF;
    IF now = TOD#08:00:00 THEN (* PLCOPEN CP-28 *)
      x := T#0S;
    END_IF;
END_PROGRAM

FUNCTION_BLOCK Stopwatch
    VAR_INPUT run : BOOL; END_VAR
    VAR_OUTPUT elapsed_ms : DINT; count : INT; END_VAR
    VAR t : TON; END_VAR
    t(IN := run, PT := T#1H);
    elapsed_ms := TIME_TO_DINT(t.ET);
    count := count + 1;
END_FUNCTION_BLOCK

FUNCTION SecondsOf : DINT
    VAR_INPUT d : TIME; END_VAR
    SecondsOf := TIME_TO_DINT(d) / 1000;
END_FUNCTION

PROGRAM integers
    VAR
        t1 : TON;
        sw : Stopwatch;
        ms, secs, n, code : DINT;
        r : REAL;
    END_VAR
    (* The rule's example: a time in integer format. *)
    IF TIME_TO_DINT(t1.ET) = 10000 THEN (* PLCOPEN CP-28 *)
      n := 0;
    END_IF;
    ms := TIME_TO_DINT(t1.ET);
    secs := ms / 1000;
    IF secs = 10 THEN (* PLCOPEN CP-28 *)
      n := 0;
    END_IF;
    IF TO_DINT(t1.ET) <> ms THEN (* PLCOPEN CP-28 *)
      n := 0;
    END_IF;
    IF sw.elapsed_ms = 500 THEN (* PLCOPEN CP-28 *)
      n := 0;
    END_IF;
    IF SecondsOf(t1.ET) = 3 THEN (* PLCOPEN CP-28 *)
      n := 0;
    END_IF;
    IF ms > 10000 THEN (* ordering comparisons are fine *)
      n := 0;
    END_IF;
    IF sw.count = 3 OR code = 7 THEN (* other integers are fine *)
      n := 0;
    END_IF;
    r := TIME_TO_REAL(t1.ET);
    IF n = 0 THEN
      n := 1;
    END_IF;
END_PROGRAM


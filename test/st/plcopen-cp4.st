FUNCTION demo : INT
    VAR_INPUT
        x1 AT %MX40 : INT; (* PLCOPEN-CP4 *)
        x2 AT %MX41 : INT; (* PLCOPEN-CP4 *)
        x3 AT %MX510 : INT;
        w1 AT %MW10 : INT; (* adjacent words don't overlap *)
        w2 AT %MW11 : INT;
        d1 AT %MW20 : DINT; (* PLCOPEN-CP4 *)
        d2 AT %MW21 : INT; (* PLCOPEN-CP4 *)
        b1 AT %MB30 : INT; (* PLCOPEN-CP4 *)
        b2 AT %MB31 : BYTE; (* PLCOPEN-CP4 *)
        i1 AT %IW50 : INT; (* different areas don't overlap *)
        q1 AT %QW50 : INT;
        s1 AT %MW60 : INT; (* PLCOPEN-CP4 *)
        s2 AT %MW60 : INT; (* PLCOPEN-CP4 *)
    END_VAR
    x1 := 42;
END_FUNCTION

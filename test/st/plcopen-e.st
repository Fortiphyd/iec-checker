CONFIGURATION cfg
  VAR_GLOBAL g_ptr : POINTER TO INT; END_VAR
  RESOURCE res ON PLC
    TASK t(INTERVAL := T#10ms, PRIORITY := 1);
    PROGRAM inst WITH t : Main;
  END_RESOURCE
END_CONFIGURATION

PROGRAM Main
  VAR_EXTERNAL g_ptr : POINTER TO INT; END_VAR
  VAR
    x : INT;
    y : INT;
    TEMP : INT;
    buf : ARRAY [0..9] OF INT;
    p : POINTER TO INT;
    q : POINTER TO INT;
    r : REF_TO INT;
    s : REF_TO INT;
    addr : DWORD;
    size : DWORD;
    i : INT;
  END_VAR
  (* E1 *)
  p := __NEW(INT, 10);   (* PLCOPEN-E1 *)
  addr := SysMemAlloc(100);   (* PLCOPEN-E1 *)
  __DELETE(p);

  (* E2 *)
  p := ADR(buf);
  p := p + 2;   (* PLCOPEN-E2 *)
  p := ADR(buf) + i * 2 + 4;   (* PLCOPEN-E2 *)
  addr := ADR(buf);
  size := addr - 16;   (* PLCOPEN-E2 *)
  g_ptr := g_ptr + 2;   (* PLCOPEN-E2 *)
  x := buf[i + 1] + p^;   (* values, not addresses *)
  x := p^ + 1;

  (* E3: the rule's example *)
  IF p^ < q^ AND p < q THEN   (* PLCOPEN-E3 *)
    TEMP := p^;
    p^ := q^;
    q^ := TEMP;
  END_IF;
  IF r >= s THEN   (* PLCOPEN-E3 *)
    x := 1;
  END_IF;
  IF addr > ADR(y) THEN   (* PLCOPEN-E3 *)
    x := 1;
  END_IF;
  IF p = q OR r <> NULL OR r = REF(x) THEN   (* equality is allowed *)
    x := 2;
  END_IF;
  IF buf[1] < buf[2] AND i < 3 THEN
    x := 3;
  END_IF;
END_PROGRAM

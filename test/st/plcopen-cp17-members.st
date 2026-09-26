TYPE pt : STRUCT x : INT; y : INT; END_STRUCT END_TYPE
FUNCTION_BLOCK f
  VAR_INPUT
    pin : pt;
    other : pt;
  END_VAR
  VAR_OUTPUT
    pout : pt;
    q : INT;
  END_VAR
  pout.x := pin.x;
  other.x := 1;
  q := other.y;
END_FUNCTION_BLOCK

CONFIGURATION cfg
  VAR_GLOBAL
    MyCalculation : REAL;
  END_VAR
  RESOURCE res ON PLC
    VAR_GLOBAL
      ResGlobal : INT;
    END_VAR
    TASK task0(INTERVAL := T#20ms, PRIORITY := 0);
    PROGRAM instance0 WITH task0 : prog0;
  END_RESOURCE
END_CONFIGURATION

PROGRAM prog0
  VAR_EXTERNAL
    MyCalculation : REAL; (* refers to the global *)
  END_VAR
  VAR
    ResGlobal : INT; (* N5 *)
    task0 : INT; (* N5 *)
  END_VAR
  MyCalculation := 1.0;
END_PROGRAM

FUNCTION_BLOCK Example
  VAR
    MyCalculation : REAL; (* N5 *)
  END_VAR
  MyCalculation := 0.0;
END_FUNCTION_BLOCK

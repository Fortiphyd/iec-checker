PROGRAM program0
  VAR
    FOR_B : INT; (* starts with a keyword, but isn't one *)
    NO_FALSE_POSITIVE : INT;
    IF_A : INT;
    TOF : INT; (* PLCOPEN-N3 *)
    OK_ALLOWED : INT;
  END_VAR

  FOR_B := IF_A / 1;
END_PROGRAM

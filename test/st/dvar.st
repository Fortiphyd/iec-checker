FUNCTION_BLOCK Example
    VAR_EXTERNAL
        global_var : INT;
    END_VAR
    global_var := 42;
END_FUNCTION_BLOCK

PROGRAM Simple
  VAR
    temp : DINT;
    i : DINT;
    unused_var AT %IW1.2 : REAL := 200.0;
    head AT %B0 : INT;
  END_VAR

  i := head;
  i := 42;
  i := 42 + head;
  i := unused_var + 42;
  i := -head;
  
END_PROGRAM

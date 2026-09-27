PROGRAM p
  VAR
    v_lt AT %MW600 : LTIME;
    after_lt AT %MW604 : INT;
    v_ldt AT %MB700 : LDT;
    after_ldt AT %MB708 : BYTE;
    v_dt AT %MB800 : DATE_AND_TIME;
    after_dt1 AT %MB808 : BYTE;
  END_VAR
  after_lt := 1;
END_PROGRAM

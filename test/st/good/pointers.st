(* References (IEC 61131-3 ed3, Table 12), and the POINTER TO and __NEW
   extensions of CODESYS and TwinCAT. *)
PROGRAM p
  VAR
    x : INT;
    r : REF_TO INT;
    q : REF_TO INT;
    p : POINTER TO INT;
  END_VAR
  r := REF(x);
  r^ := 1;
  x := r^ + 1;
  IF r <> NULL AND r = q THEN
    q := NULL;
  END_IF;
  p := ADR(x);
  p^ := 2;
  p := __NEW(INT, 10);
  __DELETE(p);
END_PROGRAM

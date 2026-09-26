PROGRAM demo
  VAR
    i : INT;
    d : DINT;
    r : REAL;
    lr : LREAL;
    w : WORD;
    t : TIME;
  END_VAR
  d := i + d; (* MixedTypeArithmetic *)
  r := r * i; (* MixedTypeArithmetic *)
  r := i * 1.5; (* MixedTypeArithmetic *)
  r := r * 2; (* integer literals take the other operand's type *)
  i := i + 1;
  lr := lr + r; (* MixedTypeArithmetic *)
  r := INT_TO_REAL(i) * r; (* explicit conversion *)
  t := t * 2; (* TIME arithmetic isn't numeric *)
  i := i + w; (* MixedTypeArithmetic *)
END_PROGRAM

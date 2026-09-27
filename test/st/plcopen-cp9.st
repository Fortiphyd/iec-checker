(* Example from PLCOpen guidelines, page 60.

   The following Function block CHARCURVE has:
   Number of Statements 18
   McCabe complexity of 12
   Prater complexity of 3,89
   Halstead complexity of 44,9
   Elshof complexity of 0,14

  Counting each statement once, including ELSIF but not ELSE, gives their 18
  statements. Their McCabe value comes out with the "plcopen" variant; the
  standard McCabe complexity is 8. CHARCURVE2 is the improved version of the
  rule's example: 12 statements, and McCabe complexity 8 (standard: 6).
 *)

FUNCTION_BLOCK CHARCURVE
  VAR_INPUT
    IN:INT;
    N:BYTE;
  END_VAR
  (* VAR_IN_OUT *)
    (* P : ARRAY [0 .. 10] OF POINT; *)
  (* END_VAR *)
  VAR_OUTPUT
    OUT:INT;
    ERR: BYTE;
  END_VAR
  VAR
    I:INT;
  END_VAR

  IF N > 1 AND N < 12 THEN
    ERR:=0;
    (* IF IN < P[0].X THEN *)
    IF IN < a THEN
      ERR := 2;
      OUT := DINT_TO_INT(a);
    (* ELSIF IN > P[N-1].X THEN *)
    ELSIF IN > a THEN
      ERR := 2;
      OUT := DINT_TO_INT(a);
    ELSE
      FOR I:=1 TO N-1 DO
        (* IF P[I-1].X >= P[I].X THEN *)
        IF a >= a THEN
          ERR:=1;
          EXIT;
        END_IF;
        (* IF IN <= P[I].X THEN *)
        IF IN <= a THEN
          EXIT;
        END_IF;
      END_FOR;
      IF ERR = 0 THEN
        (* OUT := DINT_TO_INT(P[I].Y - (P[I].X-IN) * (P[I].Y-P[I-1].Y) / (P[I].X-P[I-1].X)); *)
        OUT := DINT_TO_INT(a - (a - IN) * (a - a) / (a - a));
      ELSE
        OUT:=0;
      END_IF;
    END_IF;
  ELSE
    ERR := 4;
  END_IF;
END_FUNCTION_BLOCK

FUNCTION_BLOCK CHARCURVE2
  VAR_INPUT
    IN:INT;
    N:BYTE;
  END_VAR
  VAR_OUTPUT
    OUT:INT;
    ERR: BYTE;
  END_VAR
  VAR
    I:INT;
    iIndexFound:INT;
  END_VAR

  OUT := 0;
  ERR := 0;
  iIndexFound := -1;
  FOR I := 1 TO N-1 DO
    (* IF iIndexFound < 0 AND ERR = 0 THEN *)
    IF iIndexFound < 0 AND ERR = 0 THEN
      (* IF P[I-1].X >= P[I].X THEN *)
      IF a >= a THEN
        ERR := 1;
      (* ELSIF IN <= P[I].X THEN *)
      ELSIF IN <= a THEN
        iIndexFound := I;
      END_IF;
    END_IF;
  END_FOR;
  IF ERR = 0 THEN
    (* OUT := CalculateOut(P[iIndexFound], P[iIndexFound - 1]); *)
    OUT := a;
  ELSE
    OUT := 0;
  END_IF;
END_FUNCTION_BLOCK

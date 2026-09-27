(* Autorise le départ du wagonnet: accents in comments are fine *)
TYPE Mode : (Automatique, Arrêt); END_TYPE   (* PLCOPEN-N8 *)
TYPE Point : STRUCT x : INT; hauteur_mètres : INT; END_STRUCT; END_TYPE   (* PLCOPEN-N8 *)

FUNCTION_BLOCK Contrôle   (* PLCOPEN-N8 *)
  VAR x : INT; END_VAR
  x := 1;
END_FUNCTION_BLOCK

PROGRAM program0
  VAR
    CleanName : INT;
    another_clean_name : INT;
    départ : BOOL;   (* PLCOPEN-N8 *)
    depart : BOOL;
    double__underscore : INT;   (* PLCOPEN-N8 *)
    trailing_ : INT;   (* PLCOPEN-N8 *)
    _leading : INT;
    変数 : INT;   (* PLCOPEN-N8 *)
  END_VAR
  CleanName := another_clean_name;
  départ := TRUE;
  depart := FALSE;
END_PROGRAM

CONFIGURATION config
  RESOURCE res ON PLC
    TASK tâche(INTERVAL := T#10ms, PRIORITY := 1);   (* PLCOPEN-N8 *)
    PROGRAM instance_ WITH tâche : program0;   (* PLCOPEN-N8 *)
  END_RESOURCE
END_CONFIGURATION

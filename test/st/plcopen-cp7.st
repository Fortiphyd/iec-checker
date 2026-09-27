FUNCTION_BLOCK Scaler
  VAR_INPUT P1 : INT; P2 : INT; END_VAR
  VAR_OUTPUT Out : INT; xError : BOOL; iError : INT; END_VAR
  Out := P1 * P2;
  xError := FALSE;
  iError := 0;
END_FUNCTION_BLOCK

FUNCTION ReadSensor : INT
  VAR_INPUT channel : INT; END_VAR
  VAR_OUTPUT ErrorCode : INT; END_VAR
  ReadSensor := channel;
  ErrorCode := 0;
END_FUNCTION

FUNCTION_BLOCK Wrapper
  VAR_OUTPUT xError : BOOL; END_VAR
  VAR inner : Scaler; END_VAR
  inner(P1 := 1, P2 := 2, xError => xError);   (* passed on as its own output *)
END_FUNCTION_BLOCK

PROGRAM Main
  VAR
    Cond : BOOL;
    Eff1 : INT;
    Eff2 : INT;
    Result : INT;
    CriticalThreshold : INT;
    instance : Scaler;
    checked : Scaler;
    connected : Scaler;
    ignored : Scaler;
    err : BOOL;
    code : INT;
    unread : INT;
    unreadFlag : BOOL;
    value : INT;
    move : MC_MoveAbsolute;
    power : MC_Power;
    wrap : Wrapper;
  END_VAR
  (* The rule's example: Result is used without checking the error *)
  instance(P1 := Eff1, P2 := Eff2);   (* PLCOPEN-CP7 *)
  Result := instance.Out;
  CriticalThreshold := Result + 10000;

  (* Tested: read directly, or connected and read *)
  checked(P1 := Eff1, P2 := Eff2);
  IF NOT checked.xError AND checked.iError = 0 THEN
    CriticalThreshold := checked.Out + 10000;
  END_IF;
  connected(P1 := Eff1, P2 := Eff2, xError => err, iError => code);
  IF NOT err AND code = 0 THEN
    Result := connected.Out;
  END_IF;
  ignored(P1 := 1, P2 := 2, xError => unreadFlag, iError => unread);   (* PLCOPEN-CP7 *)

  (* Functions with an error output *)
  value := ReadSensor(channel := 1, ErrorCode => code);
  value := ReadSensor(channel := 2);   (* PLCOPEN-CP7 *)
  unread := ReadSensor(channel := 3, ErrorCode => unread);   (* PLCOPEN-CP7 *)

  (* Motion control blocks *)
  move(Execute := Cond);   (* PLCOPEN-CP7 *)
  power(Enable := TRUE);
  IF power.Error THEN
    Cond := FALSE;
  END_IF;   (* testing Error is enough *)

  (* The error of wrap is passed on as its own output *)
  wrap();
  Cond := wrap.xError;
END_PROGRAM

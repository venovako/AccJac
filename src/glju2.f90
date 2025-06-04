  ES = INT(IAND(INFO, 1), c_int)
  RT = LJU2(A11, A22, A21, CS, SN, TG, ES)
  IF (RT .LT. 0_c_int) THEN
     INFO = INT(RT)
  ELSE IF ((CS .EQ. ONE) .AND. (SN .EQ. ZERO)) THEN
     INFO = IOR(INFO, 4)
  END IF

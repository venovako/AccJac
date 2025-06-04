  ES = -1_c_int
  INFO = INT(LJV2(A11, A22, A21, CH, TH, TG, ES))
  IF (INFO .LT. 0) RETURN
  IF (IAND(INFO, 2) .NE. 0) THEN
     INFO = 2
  ELSE IF ((CH .EQ. ONE) .AND. (TH .EQ. ZERO)) THEN
     INFO = 0
  ELSE ! neither identity nor big th
     INFO = 1
  END IF
  A21 = TG

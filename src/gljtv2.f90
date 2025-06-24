  WD = (INFO .NE. 0)
  ES = -1_c_int
  TG = CH
  INFO = INT(LJV2(A11, A22, A21, CH, TH, TG, ES))
  IF (INFO .LT. 0) RETURN
  IF (IAND(INFO, 2) .NE. 0) THEN
     INFO = 2
  ELSE IF ((CH .EQ. ONE) .AND. (TH .EQ. ZERO)) THEN
     INFO = 0
  ELSE ! neither identity nor big th
     INFO = 1
  END IF
  IF (WD) THEN
     A21 = ABS(A21)
     A11 = GFMA(TG, A21, A11)
     A22 = GFMA(TG, A21, A22)
  END IF
  A21 = TG

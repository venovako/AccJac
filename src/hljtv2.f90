  WD = (INFO .NE. 0)
  ES = -1_c_int
  TG = CH
  INFO = INT(LJV2(A11, A22, A21R, A21I, CH, THR, THI, TG, ES))
  IF (INFO .LT. 0) RETURN
  IF (.NOT. WD) A21I = REAL(INFO, K)
  IF (IAND(INFO, 2) .NE. 0) THEN
     INFO = 2
  ELSE IF ((CH .EQ. ONE) .AND. (THR .EQ. ZERO) .AND. (THI .EQ. ZERO)) THEN
     INFO = 0
  ELSE ! neither identity nor big th
     INFO = 1
  END IF
  IF (WD) THEN
     A21I = CR_HYPOT(A21R, A21I)
     A11 = GFMA(TG, A21I, A11)
     A22 = GFMA(TG, A21I, A22)
  END IF
  A21R = TG

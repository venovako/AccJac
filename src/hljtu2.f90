  WD = (INFO .NE. 0)
  ES = 1_c_int
  INFO = INT(LJEU2(A11, A22, A21R, A21I, CS, TNR, TNI, TG, ES))
  IF (INFO .LT. 0) RETURN
  IF (.NOT. WD) A21I = REAL(INFO, K)
  IF ((CS .EQ. ONE) .AND. (TNR .EQ. ZERO) .AND. (TNI .EQ. ZERO)) THEN
     INFO = 0
  ELSE ! not identity
     INFO = 1
  END IF
  IF (WD) THEN
     A21I = CR_HYPOT(A21R, A21I)
     A11 = GFMA( TG, A21I, A11)
     A22 = GFMA(-TG, A21I, A22)
  END IF
  A21R = TG

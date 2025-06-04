  WD = (INFO .NE. 0)
  ES = 1_c_int
  INFO = INT(LJEU2(A11, A22, A21, CS, TN, TG, ES))
  IF (INFO .LT. 0) RETURN
  IF ((CS .EQ. ONE) .AND. (TN .EQ. ZERO)) THEN
     INFO = 0
  ELSE ! not identity
     INFO = 1
  END IF
  IF (WD) THEN
     A21 = ABS(A21)
     A11 = GFMA( TG, A21, A11)
     A22 = GFMA(-TG, A21, A22)
  END IF
  A21 = TG

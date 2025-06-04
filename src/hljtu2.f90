  ES = 1_c_int
  INFO = INT(LJEU2(A11, A22, A21R, A21I, CS, TNR, TNI, TG, ES))
  IF (INFO .LT. 0) RETURN
  IF ((CS .EQ. ONE) .AND. (TNR .EQ. ZERO) .AND. (TNI .EQ. ZERO)) THEN
     INFO = 0
  ELSE ! not identity
     INFO = 1
  END IF
  A21R =  TG
  A21I = -TG

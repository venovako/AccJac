  ES = 1_c_int
  INFO = INT(LJEV2(A11, A22, A21R, A21I, CS, TNR, TNI, L1, L2, ES))
  IF (INFO .LT. 0) RETURN
  IF ((CS .EQ. ONE) .AND. (TNR .EQ. ZERO) .AND. (TNI .EQ. ZERO)) THEN
     INFO = 5
  ELSE ! not identity
     S = INT(ES)
     IF (S .NE. 0) THEN
        A11 = SCALE(L1, S)
        A22 = SCALE(L2, S)
     ELSE ! S = 0
        A11 = L1
        A22 = L2
     END IF
     INFO = 1
  END IF

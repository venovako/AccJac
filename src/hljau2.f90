  ES = INT(IAND(INFO, 1), c_int)
  RT = LJU2(A11, A22, A21R, A21I, CS, SNR, SNI, L1, L2, ES)
  IF (RT .LT. 0_c_int) THEN
     INFO = INT(RT)
  ELSE IF ((CS .EQ. ONE) .AND. (SNR .EQ. ZERO) .AND. (SNI .EQ. ZERO)) THEN
     INFO = IOR(INFO, 4)
  ELSE ! not identity
     S = INT(ES)
     IF (S .NE. 0) THEN
        A11 = SCALE(L1, S)
        A22 = SCALE(L2, S)
     ELSE ! S = 0
        A11 = L1
        A22 = L2
     END IF
  END IF

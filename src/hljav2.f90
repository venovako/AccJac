  IF (INFO .EQ. 0) THEN
     ES =  1_c_int
  ELSE ! (ch,th)
     ES = -1_c_int
  END IF
  RT = LJV2(A11, A22, A21R, A21I, CH, SHR, SHI, TH, ES)
  IF (RT .LT. 0_c_int) THEN
     INFO = INT(RT)
  ELSE ! OK
     INFO = IAND(INT(RT), 3)
     IF ((CH .EQ. ONE) .AND. (SHR .EQ. ZERO) .AND. (SHI .EQ. ZERO)) THEN
        INFO = IOR(INFO, 4)
     ELSE ! not identity
        A = CR_HYPOT(A21R, A21I)
        A11 = TH * A + A11
        A22 = TH * A + A22
     END IF
  END IF

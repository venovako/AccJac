  IF (INFO .EQ. 0) THEN
     ES =  1_c_int
  ELSE ! (ch,th)
     ES = -1_c_int
  END IF
  TH = CH
  RT = LJV2(A11, A22, A21, CH, SH, TH, ES)
  IF (RT .LT. 0_c_int) THEN
     INFO = INT(RT)
  ELSE ! OK
     INFO = IAND(INT(RT), 3)
     IF ((CH .EQ. ONE) .AND. (SH .EQ. ZERO)) THEN
        INFO = IOR(INFO, 4)
     ELSE ! not identity
        A = ABS(A21)
        A11 = GFMA(TH, A, A11)
        A22 = GFMA(TH, A, A22)
     END IF
  END IF

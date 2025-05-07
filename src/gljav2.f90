  IF (INFO .EQ. 0) THEN
     ES = 3_c_int
  ELSE ! (ch,th)
     ES = -3_c_int
  END IF
  RT = LJV2(A11, A22, A21, CH, SH, ES)
  IF (RT .LT. 0_c_int) THEN
     INFO = INT(RT)
  ELSE ! OK
     INFO = IAND(INT(RT), 3)
     A = ABS(SH)
     IF ((.NOT. (A .LE. HUGE(A))) .OR. ((CH .EQ. ONE) .AND. (A .EQ. ZERO))) THEN
        ! identity or |TH| >= 1 => skip the transformation
        CH = ONE
        SH = ZERO
        INFO = IOR(INFO, 4)
     END IF
  END IF

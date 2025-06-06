  IF (INFO .EQ. 0) THEN
     ES = 0_c_int
  ELSE ! (ch,th)
     ES = -2_c_int
  END IF
  RT = LJV2(A11, A22, A21R, A21I, CH, SHR, SHI, TH, ES)
  IF (RT .LT. 0_c_int) THEN
     INFO = INT(RT)
  ELSE ! OK
     A = CR_HYPOT(SHR, SHI)
     IF ((.NOT. (A .LE. HUGE(A))) .OR. ((CH .EQ. ONE) .AND. (A .EQ. ZERO))) THEN
        ! identity or |TH| >= 1 => skip the transformation
        INFO = IOR(INFO, 4)
     ELSE IF (INFO .EQ. 0) THEN
        IF (A .GE. (CUTTH * CH)) THEN
           CH = CUTCH
           SHR = (SHR / A) * CUTSH
           SHI = (SHI / A) * CUTSH
           INFO = 2
        END IF
     ELSE ! SH => TH
        IF (A .GE. CUTTH) THEN
           CH = CUTCH
           SHR = (SHR / A) * CUTTH
           SHI = (SHI / A) * CUTTH
           INFO = 3
        END IF
     END IF
  END IF

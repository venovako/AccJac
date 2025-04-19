  ES = INT(IAND(INFO, 1), c_int)
  RT = LJV2(A11, A22, A21, CH, SH, ES)
  IF (RT .LT. 0_c_int) THEN
     INFO = INT(RT)
  ELSE ! OK
     A = ABS(SH)
     IF ((.NOT. (A .LE. HUGE(A))) .OR. ((CH .EQ. ONE) .AND. (A .EQ. ZERO))) THEN
        ! identity or |TH| >= 1 => skip the transformation
        CH = ONE
        SH = ZERO
        INFO = IOR(INFO, 4)
     ELSE IF (INFO .EQ. 0) THEN
        IF (A .GE. (CUTTH * CH)) THEN
           CH = CUTCH
           SH = SIGN(CUTSH, SH)
           INFO = 2
        END IF
     ELSE ! SH => TH
        IF (A .GE. CUTTH) THEN
           CH = CUTCH
           SH = SIGN(CUTTH, SH)
           INFO = 3
        END IF
     END IF
  END IF

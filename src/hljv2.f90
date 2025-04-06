  IF (INFO .GE. 0) THEN
     INFO = IAND(INFO, 1)
     A = CR_HYPOT(SHR, SHI)
     IF ((.NOT. (A .LE. HUGE(A))) .OR. ((CH .EQ. ONE) .AND. (A .EQ. ZERO))) THEN
        ! identity or |TH| >= 1 => skip the transformation
        CH = ONE
        SHR = ZERO
        SHI = ZERO
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

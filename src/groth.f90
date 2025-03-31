  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (IAND(INFO, 5) .EQ. 0) THEN
     MX = ZERO
     MY = ZERO
     INFO = 0
     DO I = 1, M
        XX = X(I) * CH + Y(I) * SH
        YY = X(I) * SH + Y(I) * CH
        X(I) = XX
        Y(I) = YY
        MX = CR_HYPOT(MX, XX)
        MY = CR_HYPOT(MY, YY)
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     MX = ZERO
     MY = ZERO
     INFO = 0
     ! SH => TH
     DO I = 1, M
        !DIR$ FMA
        XX = (X(I) + Y(I) * SH) * CH
        !DIR$ FMA
        YY = (X(I) * SH + Y(I)) * CH
        X(I) = XX
        Y(I) = YY
        MX = CR_HYPOT(MX, XX)
        MY = CR_HYPOT(MY, YY)
     END DO
  ELSE ! no-op
     INFO = 1
  END IF

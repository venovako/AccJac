  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (IAND(INFO, 5) .EQ. 0) THEN
     MX = ZERO
     MY = ZERO
     DO I = 1, M
        XX = X(I) * CH + Y(I) * SH
        YY = X(I) * SH + Y(I) * CH
        X(I) = XX
        Y(I) = YY
        GX = MAX(GX, MAX(ABS(XX), ABS(YY)))
        MX = CR_HYPOT(MX, XX)
        MY = CR_HYPOT(MY, YY)
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     MX = ZERO
     MY = ZERO
     ! SH => TH
     DO I = 1, M
        !DIR$ FMA
        XX = (Y(I) * SH + X(I)) * CH
        !DIR$ FMA
        YY = (X(I) * SH + Y(I)) * CH
        X(I) = XX
        Y(I) = YY
        GX = MAX(GX, MAX(ABS(XX), ABS(YY)))
        MX = CR_HYPOT(MX, XX)
        MY = CR_HYPOT(MY, YY)
     END DO
  END IF

  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  SH = CMPLX(SHR,  SHI, K)
  HS = CMPLX(SHR, -SHI, K)
  IF (IAND(INFO, 5) .EQ. 0) THEN
     MX = ZERO
     MY = ZERO
     DO I = 1, M
        XX = X(I) * CH + Y(I) * SH
        YY = X(I) * HS + Y(I) * CH
        X(I) = XX
        Y(I) = YY
        AX = CR_HYPOT(REAL(XX), AIMAG(YY))
        AY = CR_HYPOT(REAL(YY), AIMAG(YY))
        GX = MAX(GX, AX, AY)
        MX = CR_HYPOT(MX, AX)
        MY = CR_HYPOT(MY, AY)
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     MX = ZERO
     MY = ZERO
     ! SH => TH
     DO I = 1, M
        XX = (X(I) + Y(I) * SH) * CH
        YY = (X(I) * HS + Y(I)) * CH
        X(I) = XX
        Y(I) = YY
        AX = CR_HYPOT(REAL(XX), AIMAG(YY))
        AY = CR_HYPOT(REAL(YY), AIMAG(YY))
        GX = MAX(GX, AX, AY)
        MX = CR_HYPOT(MX, AX)
        MY = CR_HYPOT(MY, AY)
     END DO
  END IF

  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  SN = CMPLX( SNR, SNI, K)
  HS = CMPLX(-SNR, SNI, K)
  IF (IAND(INFO, 5) .EQ. 0) THEN
     MX = ZERO
     MY = ZERO
     INFO = 0
     DO I = 1, M
        XX = X(I) * CS + Y(I) * SN
        YY = X(I) * HS + Y(I) * CS
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
     INFO = 0
     ! SN => TG
     DO I = 1, M
        XX = (X(I) + Y(I) * SN) * CS
        YY = (X(I) * HS + Y(I)) * CS
        X(I) = XX
        Y(I) = YY
        AX = CR_HYPOT(REAL(XX), AIMAG(YY))
        AY = CR_HYPOT(REAL(YY), AIMAG(YY))
        GX = MAX(GX, AX, AY)
        MX = CR_HYPOT(MX, AX)
        MY = CR_HYPOT(MY, AY)
     END DO
  ELSE ! no-op
     INFO = 1
  END IF

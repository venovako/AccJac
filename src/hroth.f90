  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  MX = ZERO
  MY = ZERO
  IF (M .EQ. 0) RETURN
  SH = CMPLX(SHR,  SHI, K)
  HS = CMPLX(SHR, -SHI, K)
  IF (IAND(INFO, 5) .EQ. 0) THEN
     INFO = 0
     DO I = 1, M
        XX = X(I) * CH + Y(I) * SH
        YY = X(I) * HS + Y(I) * CH
        X(I) = XX
        Y(I) = YY
        MX = CR_HYPOT(MX, CR_HYPOT(REAL(XX), AIMAG(YY)))
        MY = CR_HYPOT(MY, CR_HYPOT(REAL(YY), AIMAG(YY)))
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     INFO = 0
     ! SH => TH
     DO I = 1, M
        XX = (X(I) + Y(I) * SH) * CH
        YY = (X(I) * HS + Y(I)) * CH
        X(I) = XX
        Y(I) = YY
        MX = CR_HYPOT(MX, CR_HYPOT(REAL(XX), AIMAG(YY)))
        MY = CR_HYPOT(MY, CR_HYPOT(REAL(YY), AIMAG(YY)))
     END DO
  ELSE ! no-op
     INFO = 1
  END IF

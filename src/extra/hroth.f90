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
        AX = CR_HYPOT(REAL(XX), AIMAG(XX))
        AY = CR_HYPOT(REAL(YY), AIMAG(YY))
        GX = MAX(GX, MAX(AX, AY))
        MX = CR_HYPOT(MX, AX)
        MY = CR_HYPOT(MY, AY)
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     MX = ZERO
     MY = ZERO
     ! SH => TH
     DO I = 1, M
        ! XX = (Y(I) * SH + X(I)) * CH
        XX = CMPLX(&
             (REAL(Y(I)) * REAL(SH) + (REAL(X(I)) - AIMAG(Y(I)) * AIMAG(SH))) * CH,&
             (REAL(Y(I)) * AIMAG(SH) + (AIMAG(X(I)) + AIMAG(Y(I)) * REAL(SH))) * CH, K)
        ! YY = (X(I) * HS + Y(I)) * CH
        YY = CMPLX(&
             (REAL(X(I)) * REAL(HS) + (REAL(Y(I)) - AIMAG(X(I)) * AIMAG(HS))) * CH,&
             (REAL(X(I)) * AIMAG(HS) + (AIMAG(Y(I)) + AIMAG(X(I)) * REAL(HS))) * CH, K)
        X(I) = XX
        Y(I) = YY
        AX = CR_HYPOT(REAL(XX), AIMAG(XX))
        AY = CR_HYPOT(REAL(YY), AIMAG(YY))
        GX = MAX(GX, MAX(AX, AY))
        MX = CR_HYPOT(MX, AX)
        MY = CR_HYPOT(MY, AY)
     END DO
  END IF

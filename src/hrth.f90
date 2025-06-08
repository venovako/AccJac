#ifndef NDEBUG
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  TH = CMPLX(THR, THI, K)
  IF (INFO .EQ. 0) THEN
     DO I = 1, M
        ! XX = (Y(I) * TH + X(I)) * CH
        !DIR$ FMA
        XX = CMPLX((REAL(Y(I)) * REAL(TH) + (REAL(X(I)) - AIMAG(Y(I)) * AIMAG(TH))) * CH,&
             (REAL(Y(I)) * AIMAG(TH) + (AIMAG(X(I)) + AIMAG(Y(I)) * REAL(TH))) * CH, K)
        ! YY = (X(I) * CONJG(TH) + Y(I)) * CH
        !DIR$ FMA
        YY = CMPLX((REAL(X(I)) * REAL(TH) + (REAL(Y(I)) + AIMAG(X(I)) * AIMAG(TH))) * CH,&
             ((AIMAG(Y(I)) + AIMAG(X(I)) * REAL(TH)) - REAL(X(I)) * AIMAG(TH)) * CH, K)
        X(I) = XX
        Y(I) = YY
     END DO
  ELSE ! INFO .NE. 0
     DO I = 1, M
        ! XX = (Y(I) * TH + X(I)) * CH
        !DIR$ FMA
        XX = CMPLX((REAL(Y(I)) * REAL(TH) + (REAL(X(I)) - AIMAG(Y(I)) * AIMAG(TH))) * CH,&
             (REAL(Y(I)) * AIMAG(TH) + (AIMAG(X(I)) + AIMAG(Y(I)) * REAL(TH))) * CH, K)
        ! YY = (X(I) * CONJG(TH) + Y(I)) * CH
        !DIR$ FMA
        YY = CMPLX((REAL(X(I)) * REAL(TH) + (REAL(Y(I)) + AIMAG(X(I)) * AIMAG(TH))) * CH,&
             ((AIMAG(Y(I)) + AIMAG(X(I)) * REAL(TH)) - REAL(X(I)) * AIMAG(TH)) * CH, K)
        X(I) = XX
        Y(I) = YY
        GX = MAX(GX, MAX(MAX(ABS(REAL(XX)), ABS(AIMAG(XX))), MAX(ABS(REAL(YY)), ABS(AIMAG(YY)))))
     END DO
  END IF

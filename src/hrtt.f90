#ifndef NDEBUG
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  TN = CMPLX(TNR, TNI, K)
  IF (INFO .EQ. 0) THEN
     DO I = 1, M
        ! XX = (Y(I) * TN + X(I)) * CS
        !DIR$ FMA
        XX = CMPLX((REAL(Y(I)) * REAL(TN) + (REAL(X(I)) - AIMAG(Y(I)) * AIMAG(TN))) * CS,&
             (REAL(Y(I)) * AIMAG(TN) + (AIMAG(X(I)) + AIMAG(Y(I)) * REAL(TN))) * CS, K)
        ! YY = (Y(I) - X(I) * CONJG(TN)) * CS
        !DIR$ FMA
        YY = CMPLX(((REAL(Y(I)) - AIMAG(X(I)) * AIMAG(TN)) - REAL(X(I)) * REAL(TN)) * CS,&
             (REAL(X(I)) * AIMAG(TN) + (AIMAG(Y(I)) - AIMAG(X(I)) * REAL(TN))) * CS, K)
        X(I) = XX
        Y(I) = YY
     END DO
  ELSE ! INFO .NE. 0
     DO I = 1, M
        ! XX = (Y(I) * TN + X(I)) * CS
        !DIR$ FMA
        XX = CMPLX((REAL(Y(I)) * REAL(TN) + (REAL(X(I)) - AIMAG(Y(I)) * AIMAG(TN))) * CS,&
             (REAL(Y(I)) * AIMAG(TN) + (AIMAG(X(I)) + AIMAG(Y(I)) * REAL(TN))) * CS, K)
        ! YY = (Y(I) - X(I) * CONJG(TN)) * CS
        !DIR$ FMA
        YY = CMPLX(((REAL(Y(I)) - AIMAG(X(I)) * AIMAG(TN)) - REAL(X(I)) * REAL(TN)) * CS,&
             (REAL(X(I)) * AIMAG(TN) + (AIMAG(Y(I)) - AIMAG(X(I)) * REAL(TN))) * CS, K)
        X(I) = XX
        Y(I) = YY
        GX = MAX(GX, MAX(MAX(ABS(REAL(XX)), ABS(AIMAG(XX))), MAX(ABS(REAL(YY)), ABS(AIMAG(YY)))))
     END DO
  END IF

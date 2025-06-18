#ifndef NDEBUG
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  TN = CMPLX( TNR, TNI, K)
  NT = CMPLX(-TNR, TNI, K)
  IF (INFO .EQ. 0) THEN
     DO I = 1, M
        ! XX = (Y(I) * TN + X(I)) * CS
        ! YY = (Y(I) - X(I) * CONJG(TN)) * CS
        XX = HFMA(Y(I), TN, X(I))
        YY = HFMA(X(I), NT, Y(I))
        XX = CMPLX(REAL(XX) * CS, AIMAG(XX) * CS, K)
        YY = CMPLX(REAL(YY) * CS, AIMAG(YY) * CS, K)
        X(I) = XX
        Y(I) = YY
     END DO
  ELSE ! INFO .NE. 0
     DO I = 1, M
        ! XX = (Y(I) * TN + X(I)) * CS
        ! YY = (Y(I) - X(I) * CONJG(TN)) * CS
        XX = HFMA(Y(I), TN, X(I))
        YY = HFMA(X(I), NT, Y(I))
        XX = CMPLX(REAL(XX) * CS, AIMAG(XX) * CS, K)
        YY = CMPLX(REAL(YY) * CS, AIMAG(YY) * CS, K)
        X(I) = XX
        Y(I) = YY
        GX = MAX(GX, MAX(MAX(ABS(REAL(XX)), ABS(AIMAG(XX))), MAX(ABS(REAL(YY)), ABS(AIMAG(YY)))))
     END DO
  END IF

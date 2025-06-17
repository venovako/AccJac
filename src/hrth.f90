#ifndef NDEBUG
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  TH = CMPLX(THR,  THI, K)
  HT = CMPLX(THR, -THI, K)
  IF (INFO .EQ. 0) THEN
     DO I = 1, M
        ! XX = (Y(I) * TH + X(I)) * CH
        ! YY = (X(I) * CONJG(TH) + Y(I)) * CH
        XX = HFMA(Y(I), TH, X(I))
        YY = HFMA(X(I), HT, Y(I))
        XX = CMPLX(REAL(XX) * CH, AIMAG(XX) * CH, K)
        YY = CMPLX(REAL(YY) * CH, AIMAG(YY) * CH, K)
        X(I) = XX
        Y(I) = YY
     END DO
  ELSE ! INFO .NE. 0
     DO I = 1, M
        ! XX = (Y(I) * TH + X(I)) * CH
        ! YY = (X(I) * CONJG(TH) + Y(I)) * CH
        XX = HFMA(Y(I), TH, X(I))
        YY = HFMA(X(I), HT, Y(I))
        XX = CMPLX(REAL(XX) * CH, AIMAG(XX) * CH, K)
        YY = CMPLX(REAL(YY) * CH, AIMAG(YY) * CH, K)
        X(I) = XX
        Y(I) = YY
        GX = MAX(GX, MAX(MAX(ABS(REAL(XX)), ABS(AIMAG(XX))), MAX(ABS(REAL(YY)), ABS(AIMAG(YY)))))
     END DO
  END IF

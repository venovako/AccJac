#ifndef NDEBUG
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  TG = CMPLX(TGR, TGI, K)
  IF (INFO .EQ. 0) THEN
     DO I = 1, M
        ! XX = (Y(I) * TG + X(I)) * CS
        !DIR$ FMA
        XX = CMPLX((REAL(Y(I)) * REAL(TG) + (REAL(X(I)) - AIMAG(Y(I)) * AIMAG(TG))) * CS,&
             (REAL(Y(I)) * AIMAG(TG) + (AIMAG(X(I)) + AIMAG(Y(I)) * REAL(TG))) * CS, K)
        ! YY = (Y(I) - X(I) * CONJG(TG)) * CS
        !DIR$ FMA
        YY = CMPLX(((REAL(Y(I)) - AIMAG(X(I)) * AIMAG(TG)) - REAL(X(I)) * REAL(TG)) * CS,&
             (REAL(X(I)) * AIMAG(TG) + (AIMAG(Y(I)) - AIMAG(X(I)) * REAL(TG))) * CS, K)
        X(I) = XX
        Y(I) = YY
     END DO
  ELSE ! INFO .NE. 0
     DO I = 1, M
        ! XX = (Y(I) * TG + X(I)) * CS
        !DIR$ FMA
        XX = CMPLX((REAL(Y(I)) * REAL(TG) + (REAL(X(I)) - AIMAG(Y(I)) * AIMAG(TG))) * CS,&
             (REAL(Y(I)) * AIMAG(TG) + (AIMAG(X(I)) + AIMAG(Y(I)) * REAL(TG))) * CS, K)
        ! YY = (Y(I) - X(I) * CONJG(TG)) * CS
        !DIR$ FMA
        YY = CMPLX(((REAL(Y(I)) - AIMAG(X(I)) * AIMAG(TG)) - REAL(X(I)) * REAL(TG)) * CS,&
             (REAL(X(I)) * AIMAG(TG) + (AIMAG(Y(I)) - AIMAG(X(I)) * REAL(TG))) * CS, K)
        X(I) = XX
        Y(I) = YY
        GX = MAX(GX, REAL(XX), AIMAG(XX), REAL(YY), AIMAG(YY))
     END DO
  END IF

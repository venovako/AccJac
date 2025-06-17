#ifndef NDEBUG
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  IF (INFO .EQ. 0) THEN
     DO I = 1, M
        XX = GFMA(Y(I), TH, X(I)) * CH
        YY = GFMA(X(I), TH, Y(I)) * CH
        X(I) = XX
        Y(I) = YY
     END DO
  ELSE ! INFO .NE. 0
     DO I = 1, M
        XX = GFMA(Y(I), TH, X(I)) * CH
        YY = GFMA(X(I), TH, Y(I)) * CH
        X(I) = XX
        Y(I) = YY
        GX = MAX(GX, MAX(ABS(XX), ABS(YY)))
     END DO
  END IF

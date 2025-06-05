#ifndef NDEBUG
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  IF (INFO .EQ. 0) THEN
     DO I = 1, M
        !DIR$ FMA
        XX = (X(I) + Y(I) * TG) * CS
        !DIR$ FMA
        YY = (Y(I) - X(I) * TG) * CS
        X(I) = XX
        Y(I) = YY
     END DO
  ELSE ! INFO .NE. 0
     DO I = 1, M
        !DIR$ FMA
        XX = (X(I) + Y(I) * TG) * CS
        !DIR$ FMA
        YY = (Y(I) - X(I) * TG) * CS
        X(I) = XX
        Y(I) = YY
        GX = MAX(GX, ABS(XX), ABS(YY))
     END DO
  END IF

  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  SN = CMPLX( SNR, SNI, K)
  HS = CMPLX(-SNR, SNI, K)
  IF (IAND(INFO, 5) .EQ. 0) THEN
     IF (IAND(INFO, 8) .EQ. 0) THEN
        DO I = 1, M
           XX = X(I) * CS + Y(I) * SN
           YY = X(I) * HS + Y(I) * CS
           X(I) = XX
           Y(I) = YY
        END DO
     ELSE ! swap
        DO I = 1, M
           XX = X(I) * CS + Y(I) * SN
           YY = X(I) * HS + Y(I) * CS
           X(I) = YY
           Y(I) = XX
        END DO
     END IF
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SN => TG
     IF (IAND(INFO, 8) .EQ. 0) THEN
        DO I = 1, M
           XX = (X(I) + Y(I) * SN) * CS
           YY = (X(I) * HS + Y(I)) * CS
           X(I) = XX
           Y(I) = YY
        END DO
     ELSE ! swap
        DO I = 1, M
           XX = (X(I) + Y(I) * SN) * CS
           YY = (X(I) * HS + Y(I)) * CS
           X(I) = YY
           Y(I) = XX
        END DO
     END IF
  ELSE IF (IAND(INFO, 8) .NE. 0) THEN
     DO I = 1, M
        XX = X(I)
        YY = Y(I)
        X(I) = YY
        Y(I) = XX
     END DO
  END IF

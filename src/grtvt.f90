  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (IAND(INFO, 5) .EQ. 0) THEN
     IF (IAND(INFO, 8) .EQ. 0) THEN
        DO I = 1, N
           XX = (X(I) * CS) + (Y(I) * SN)
           YY = (Y(I) * CS) - (X(I) * SN)
           X(I) = XX
           Y(I) = YY
        END DO
     ELSE ! swap
        DO I = 1, N
           XX = (X(I) * CS) + (Y(I) * SN)
           YY = (Y(I) * CS) - (X(I) * SN)
           X(I) = YY
           Y(I) = XX
        END DO
     END IF
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SN => TG
     NS = -SN
     IF (IAND(INFO, 8) .EQ. 0) THEN
        DO I = 1, N
           XX = GFMA(Y(I), SN, X(I)) * CS
           YY = GFMA(X(I), NS, Y(I)) * CS
           X(I) = XX
           Y(I) = YY
        END DO
     ELSE ! swap
        DO I = 1, N
           XX = GFMA(Y(I), SN, X(I)) * CS
           YY = GFMA(X(I), NS, Y(I)) * CS
           X(I) = YY
           Y(I) = XX
        END DO
     END IF
  ELSE IF (IAND(INFO, 8) .NE. 0) THEN
     DO I = 1, N
        XX = X(I)
        YY = Y(I)
        X(I) = YY
        Y(I) = XX
     END DO
  END IF

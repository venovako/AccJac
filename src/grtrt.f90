  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (IAND(INFO, 5) .EQ. 0) THEN
     IF (IAND(INFO, 8) .EQ. 0) THEN
        DO I = 1, N
           XX = A(I,P) * CS + A(I,Q) * SN
           YY = A(I,Q) * CS - A(I,P) * SN
           A(I,P) = XX
           A(I,Q) = YY
           AX = MAX(AX, ABS(XX), ABS(YY))
        END DO
     ELSE ! swap
        DO I = 1, N
           XX = A(I,P) * CS + A(I,Q) * SN
           YY = A(I,Q) * CS - A(I,P) * SN
           A(I,P) = YY
           A(I,Q) = XX
           AX = MAX(AX, ABS(XX), ABS(YY))
        END DO
     END IF
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SN => TG
     IF (IAND(INFO, 8) .EQ. 0) THEN
        DO I = 1, N
           !DIR$ FMA
           XX = (A(I,P) + A(I,Q) * SN) * CS
           !DIR$ FMA
           YY = (A(I,Q) - A(I,P) * SN) * CS
           A(I,P) = XX
           A(I,Q) = YY
           AX = MAX(AX, ABS(XX), ABS(YY))
        END DO
     ELSE ! swap
        DO I = 1, N
           !DIR$ FMA
           XX = (A(I,P) + A(I,Q) * SN) * CS
           !DIR$ FMA
           YY = (A(I,Q) - A(I,P) * SN) * CS
           A(I,P) = YY
           A(I,Q) = XX
           AX = MAX(AX, ABS(XX), ABS(YY))
        END DO
     END IF
  ELSE IF (IAND(INFO, 8) .NE. 0) THEN
     DO I = 1, N
        XX = A(I,P)
        YY = A(I,Q)
        A(I,P) = YY
        A(I,Q) = XX
     END DO
  END IF

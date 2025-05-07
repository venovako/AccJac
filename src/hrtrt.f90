  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  SN = CMPLX( SNR, SNI, K)
  HS = CMPLX(-SNR, SNI, K)
  IF (IAND(INFO, 5) .EQ. 0) THEN
     IF (IAND(INFO, 8) .EQ. 0) THEN
        DO I = 1, N
           XX = A(I,P) * CS + A(I,Q) * SN
           YY = A(I,P) * HS + A(I,Q) * CS
           A(I,P) = XX
           A(I,Q) = YY
        END DO
     ELSE ! swap
        DO I = 1, N
           XX = A(I,P) * CS + A(I,Q) * SN
           YY = A(I,P) * HS + A(I,Q) * CS
           A(I,P) = YY
           A(I,Q) = XX
        END DO
     END IF
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SN => TG
     IF (IAND(INFO, 8) .EQ. 0) THEN
        DO I = 1, N
           XX = (A(I,P) + A(I,Q) * SN) * CS
           YY = (A(I,P) * HS + A(I,Q)) * CS
           A(I,P) = XX
           A(I,Q) = YY
        END DO
     ELSE ! swap
        DO I = 1, N
           XX = (A(I,P) + A(I,Q) * SN) * CS
           YY = (A(I,P) * HS + A(I,Q)) * CS
           A(I,P) = YY
           A(I,Q) = XX
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

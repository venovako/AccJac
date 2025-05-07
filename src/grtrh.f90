  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (IAND(INFO, 5) .EQ. 0) THEN
     DO I = 1, N
        XX = A(I,P) * CH + A(I,Q) * SH
        YY = A(I,P) * SH + A(I,Q) * CH
        A(I,P) = XX
        A(I,Q) = YY
        AX = MAX(AX, ABS(XX), ABS(YY))
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SH => TH
     DO I = 1, N
        !DIR$ FMA
        XX = (A(I,P) + A(I,Q) * SH) * CH
        !DIR$ FMA
        YY = (A(I,P) * SH + A(I,Q)) * CH
        A(I,P) = XX
        A(I,Q) = YY
        AX = MAX(AX, ABS(XX), ABS(YY))
     END DO
  END IF

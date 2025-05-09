  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (IAND(INFO, 5) .EQ. 0) THEN
     DO J = 1, N
        XX = CH * A(P,J) + SH * A(Q,J)
        YY = SH * A(P,J) + CH * A(Q,J)
        A(P,J) = XX
        A(Q,J) = YY
        AX = MAX(AX, ABS(XX), ABS(YY))
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SH => TH
     DO J = 1, N
        !DIR$ FMA
        XX = CH * (A(P,J) + SH * A(Q,J))
        !DIR$ FMA
        YY = CH * (SH * A(P,J) + A(Q,J))
        A(P,J) = XX
        A(Q,J) = YY
        AX = MAX(AX, ABS(XX), ABS(YY))
     END DO
  END IF

  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (IAND(INFO, 5) .EQ. 0) THEN
     DO J = 1, P-1
        XX = CH * A(P,J) + SH * A(Q,J)
        YY = SH * A(P,J) + CH * A(Q,J)
        A(P,J) = XX
        A(Q,J) = YY
        AX = MAX(AX, MAX(ABS(XX), ABS(YY)))
     END DO
     A(Q,P) = SH * A(P,P) + CH * A(Q,P)
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SH => TH
     DO J = 1, P-1
        XX = CH * GFMA(SH, A(Q,J), A(P,J))
        YY = CH * GFMA(SH, A(P,J), A(Q,J))
        A(P,J) = XX
        A(Q,J) = YY
        AX = MAX(AX, MAX(ABS(XX), ABS(YY)))
     END DO
     A(Q,P) = CH * GFMA(SH, A(P,P), A(Q,P))
  END IF

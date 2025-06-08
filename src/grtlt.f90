  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (IAND(INFO, 5) .EQ. 0) THEN
     DO J = 1, P-1
        XX = CS * A(P,J) + SN * A(Q,J)
        YY = CS * A(Q,J) - SN * A(P,J)
        A(P,J) = XX
        A(Q,J) = YY
        AX = MAX(AX, MAX(ABS(XX), ABS(YY)))
     END DO
     A(Q,P) = CS * A(Q,P) - SN * A(P,P)
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SN => TG
     DO J = 1, P-1
        XX = CS * GFMA( SN, A(Q,J), A(P,J))
        YY = CS * GFMA(-SN, A(P,J), A(Q,J))
        A(P,J) = XX
        A(Q,J) = YY
        AX = MAX(AX, MAX(ABS(XX), ABS(YY)))
     END DO
     A(Q,P) = CS * GFMA(-SN, A(P,P), A(Q,P))
  END IF

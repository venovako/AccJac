  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  SN = CMPLX(-SNR, -SNI, K)
  HS = CMPLX( SNR, -SNI, K)
  IF (IAND(INFO, 5) .EQ. 0) THEN
     DO J = 1, P-1
        XX = HS * A(Q,J) + CS * A(P,J)
        YY = SN * A(P,J) + CS * A(Q,J)
        A(P,J) = XX
        A(Q,J) = YY
        AX = MAX(AX, MAX(CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY))))
     END DO
     A(Q,P) = SN * A(P,P) + CS * A(Q,P)
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SN => TG
     DO J = 1, P-1
        XX = CS * HFMA(HS, A(Q,J), A(P,J))
        YY = CS * HFMA(SN, A(P,J), A(Q,J))
        A(P,J) = XX
        A(Q,J) = YY
        AX = MAX(AX, MAX(CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY))))
     END DO
     A(Q,P) = CS * HFMA(SN, A(P,P), A(Q,P))
  END IF

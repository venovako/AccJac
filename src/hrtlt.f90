  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  SN = CMPLX(-SNR, -SNI, K)
  HS = CMPLX( SNR, -SNI, K)
  IF (IAND(INFO, 5) .EQ. 0) THEN
     IF (IAND(INFO, 8) .EQ. 0) THEN
        DO J = 1, N
           XX = CS * A(P,J) + HS * A(Q,J)
           YY = SN * A(P,J) + CS * A(Q,J)
           A(P,J) = XX
           A(Q,J) = YY
           AX = MAX(AX, CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY)))
        END DO
     ELSE ! swap
        DO J = 1, N
           XX = CS * A(P,J) + HS * A(Q,J)
           YY = SN * A(P,J) + CS * A(Q,J)
           A(P,J) = YY
           A(Q,J) = XX
           AX = MAX(AX, CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY)))
        END DO
     END IF
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SN => TG
     IF (IAND(INFO, 8) .EQ. 0) THEN
        DO J = 1, N
           !XX = CS * (A(P,J) + HS * A(Q,J))
           XX = CS * CXFMA(HS, A(Q,J), A(P,J))
           !YY = CS * (SN * A(P,J) + A(Q,J))
           YY = CS * CXFMA(SN, A(P,J), A(Q,J))
           A(P,J) = XX
           A(Q,J) = YY
           AX = MAX(AX, CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY)))
        END DO
     ELSE ! swap
        DO J = 1, N
           !XX = CS * (A(P,J) + HS * A(Q,J))
           XX = CS * CXFMA(HS, A(Q,J), A(P,J))
           !YY = CS * (SN * A(P,J) + A(Q,J))
           YY = CS * CXFMA(SN, A(P,J), A(Q,J))
           A(P,J) = YY
           A(Q,J) = XX
           AX = MAX(AX, CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY)))
        END DO
     END IF
  ELSE IF (IAND(INFO, 8) .NE. 0) THEN
     DO J = 1, N
        XX = A(P,J)
        YY = A(Q,J)
        A(P,J) = YY
        A(Q,J) = XX
     END DO
  END IF

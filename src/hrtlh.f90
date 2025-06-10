  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  SH = CMPLX(SHR,  SHI, K)
  HS = CMPLX(SHR, -SHI, K)
  IF (IAND(INFO, 5) .EQ. 0) THEN
     DO J = 1, P-1
        XX = HS * A(Q,J) + CH * A(P,J)
        YY = SH * A(P,J) + CH * A(Q,J)
        A(P,J) = XX
        A(Q,J) = YY
        AX = MAX(AX, MAX(CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY))))
     END DO
     A(Q,P) = SH * A(P,P) + CH * A(Q,P)
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SH => TH
     DO J = 1, P-1
        XX = CH * HFMA(HS, A(Q,J), A(P,J))
        YY = CH * HFMA(SH, A(P,J), A(Q,J))
        A(P,J) = XX
        A(Q,J) = YY
        AX = MAX(AX, MAX(CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY))))
     END DO
     A(Q,P) = CH * HFMA(SH, A(P,P), A(Q,P))
  END IF

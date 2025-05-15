  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  SH = CMPLX(SHR,  SHI, K)
  HS = CMPLX(SHR, -SHI, K)
  IF (IAND(INFO, 5) .EQ. 0) THEN
     DO J = 1, N
        XX = CH * A(P,J) + HS * A(Q,J)
        YY = SH * A(P,J) + CH * A(Q,J)
        A(P,J) = XX
        A(Q,J) = YY
        AX = MAX(AX, CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY)))
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SH => TH
     DO J = 1, N
        XX = CH * (A(P,J) + HS * A(Q,J))
        YY = CH * (SH * A(P,J) + A(Q,J))
        A(P,J) = XX
        A(Q,J) = YY
        AX = MAX(AX, CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY)))
     END DO
  END IF

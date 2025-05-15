  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  SH = CMPLX(SHR,  SHI, K)
  HS = CMPLX(SHR, -SHI, K)
  IF (IAND(INFO, 5) .EQ. 0) THEN
     DO I = 1, N
        XX = A(I,P) * CH + A(I,Q) * SH
        YY = A(I,P) * HS + A(I,Q) * CH
        A(I,P) = XX
        A(I,Q) = YY
        AX = MAX(AX, CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY)))
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SH => TH
     DO I = 1, N
        XX = (A(I,Q) * SH + A(I,P)) * CH
        YY = (A(I,P) * HS + A(I,Q)) * CH
        A(I,P) = XX
        A(I,Q) = YY
        AX = MAX(AX, CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY)))
     END DO
  END IF

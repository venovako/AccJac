  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  SH = CMPLX(SHR,  SHI, K)
  HS = CMPLX(SHR, -SHI, K)
  IF (IAND(INFO, 5) .EQ. 0) THEN
     YY = CMPLX(REAL(A(Q,P)), -AIMAG(A(Q,P)), K)
     A(P,P) = (YY * SH) + (A(P,P) * CH)
     DO I = P+1, Q-1
        YY = CMPLX(REAL(A(Q,I)), -AIMAG(A(Q,I)), K)
        XX = (YY * SH) + (A(I,P) * CH)
        YY = (A(I,P) * HS) + (YY * CH)
        A(I,P) = XX
        A(Q,I) = CMPLX(REAL(YY), -AIMAG(YY), K)
        AX = MAX(AX, MAX(CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY))))
     END DO
     A(Q,P) = (A(Q,Q) * SH) + (A(Q,P) * CH)
     DO I = Q+1, N
        XX = (A(I,Q) * SH) + (A(I,P) * CH)
        YY = (A(I,P) * HS) + (A(I,Q) * CH)
        A(I,P) = XX
        A(I,Q) = YY
        AX = MAX(AX, MAX(CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY))))
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SH => TH
     YY = CMPLX(REAL(A(Q,P)), -AIMAG(A(Q,P)), K)
     A(P,P) = HFMA(YY, SH, A(P,P)) * CH
     DO I = P+1, Q-1
        YY = CMPLX(REAL(A(Q,I)), -AIMAG(A(Q,I)), K)
        XX = HFMA(YY, SH, A(I,P)) * CH
        YY = HFMA(A(I,P), HS, YY) * CH
        A(I,P) = XX
        A(Q,I) = CMPLX(REAL(YY), -AIMAG(YY), K)
        AX = MAX(AX, MAX(CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY))))
     END DO
     A(Q,P) = HFMA(A(Q,Q), SH, A(Q,P)) * CH
     DO I = Q+1, N
        XX = HFMA(A(I,Q), SH, A(I,P)) * CH
        YY = HFMA(A(I,P), HS, A(I,Q)) * CH
        A(I,P) = XX
        A(I,Q) = YY
        AX = MAX(AX, MAX(CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY))))
     END DO
  END IF

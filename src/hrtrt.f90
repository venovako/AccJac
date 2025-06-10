  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  SN = CMPLX( SNR, SNI, K)
  HS = CMPLX(-SNR, SNI, K)
  IF (IAND(INFO, 5) .EQ. 0) THEN
     YY = CMPLX(REAL(A(Q,P)), -AIMAG(A(Q,P)), K)
     A(P,P) = YY * SN + A(P,P) * CS
     DO I = P+1, Q-1
        YY = CMPLX(REAL(A(Q,I)), -AIMAG(A(Q,I)), K)
        XX = YY * SN + A(I,P) * CS
        YY = A(I,P) * HS + YY * CS
        A(I,P) = XX
        A(Q,I) = CMPLX(REAL(YY), -AIMAG(YY), K)
        AX = MAX(AX, MAX(CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY))))
     END DO
     A(Q,P) = A(Q,Q) * SN + A(Q,P) * CS
     DO I = Q+1, N
        XX = A(I,Q) * SN + A(I,P) * CS
        YY = A(I,P) * HS + A(I,Q) * CS
        A(I,P) = XX
        A(I,Q) = YY
        AX = MAX(AX, MAX(CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY))))
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SN => TG
     YY = CMPLX(REAL(A(Q,P)), -AIMAG(A(Q,P)), K)
     A(P,P) = HFMA(YY, SN, A(P,P)) * CS
     DO I = P+1, Q-1
        YY = CMPLX(REAL(A(Q,I)), -AIMAG(A(Q,I)), K)
        XX = HFMA(YY, SN, A(I,P)) * CS
        YY = HFMA(A(I,P), HS, YY) * CS
        A(I,P) = XX
        A(Q,I) = CMPLX(REAL(YY), -AIMAG(YY), K)
        AX = MAX(AX, MAX(CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY))))
     END DO
     A(Q,P) = HFMA(A(Q,Q), SN, A(Q,P)) * CS
     DO I = Q+1, N
        XX = HFMA(A(I,Q), SN, A(I,P)) * CS
        YY = HFMA(A(I,P), HS, A(I,Q)) * CS
        A(I,P) = XX
        A(I,Q) = YY
        AX = MAX(AX, MAX(CR_HYPOT(REAL(XX), AIMAG(XX)), CR_HYPOT(REAL(YY), AIMAG(YY))))
     END DO
  END IF

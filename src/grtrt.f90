  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (IAND(INFO, 5) .EQ. 0) THEN
     A(P,P) = (A(P,P) * CS) + (A(Q,P) * SN)
     DO I = P+1, Q-1
        XX = (A(I,P) * CS) + (A(Q,I) * SN)
        YY = (A(Q,I) * CS) - (A(I,P) * SN)
        A(I,P) = XX
        A(Q,I) = YY
        AX = MAX(AX, MAX(ABS(XX), ABS(YY)))
     END DO
     A(Q,P) = (A(Q,Q) * SN) + (A(Q,P) * CS)
     DO I = Q+1, N
        XX = (A(I,P) * CS) + (A(I,Q) * SN)
        YY = (A(I,Q) * CS) - (A(I,P) * SN)
        A(I,P) = XX
        A(I,Q) = YY
        AX = MAX(AX, MAX(ABS(XX), ABS(YY)))
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SN => TG
     A(P,P) = GFMA(A(Q,P), SN, A(P,P)) * CS
     DO I = P+1, Q-1
        XX = GFMA(A(Q,I),  SN, A(I,P)) * CS
        YY = GFMA(A(I,P), -SN, A(Q,I)) * CS
        A(I,P) = XX
        A(Q,I) = YY
        AX = MAX(AX, MAX(ABS(XX), ABS(YY)))
     END DO
     A(Q,P) = GFMA(A(Q,Q), SN, A(Q,P)) * CS
     DO I = Q+1, N
        XX = GFMA(A(I,Q),  SN, A(I,P)) * CS
        YY = GFMA(A(I,P), -SN, A(I,Q)) * CS
        A(I,P) = XX
        A(I,Q) = YY
        AX = MAX(AX, MAX(ABS(XX), ABS(YY)))
     END DO
  END IF

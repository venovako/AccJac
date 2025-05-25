  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (IAND(INFO, 5) .EQ. 0) THEN
     A(P,P) = A(P,P) * CH + A(Q,P) * SH
     DO I = P+1, Q-1
        XX = A(I,P) * CH + A(Q,I) * SH
        YY = A(I,P) * SH + A(Q,I) * CH
        A(I,P) = XX
        A(Q,I) = YY
        AX = MAX(AX, ABS(XX), ABS(YY))
     END DO
     A(Q,P) = A(Q,Q) * SH + A(Q,P) * CH
     DO I = Q+1, N
        XX = A(I,P) * CH + A(I,Q) * SH
        YY = A(I,P) * SH + A(I,Q) * CH
        A(I,P) = XX
        A(I,Q) = YY
        AX = MAX(AX, ABS(XX), ABS(YY))
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SH => TH
     A(P,P) = GFMA(A(Q,P), SH, A(P,P)) * CH
     DO I = P+1, Q-1
        XX = GFMA(A(Q,I), SH, A(I,P)) * CH
        YY = GFMA(A(I,P), SH, A(Q,I)) * CH
        A(I,P) = XX
        A(Q,I) = YY
        AX = MAX(AX, ABS(XX), ABS(YY))
     END DO
     A(Q,P) = GFMA(A(Q,Q), SH, A(Q,P)) * CH
     DO I = Q+1, N
        XX = GFMA(A(I,Q), SH, A(I,P)) * CH
        YY = GFMA(A(I,P), SH, A(I,Q)) * CH
        A(I,P) = XX
        A(I,Q) = YY
        AX = MAX(AX, ABS(XX), ABS(YY))
     END DO
  END IF

  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  SH = CMPLX(SHR,  SHI, K)
  HS = CMPLX(SHR, -SHI, K)
  IF (IAND(INFO, 5) .EQ. 0) THEN
     DO I = 1, N
        XX = (X(I) * CH) + (Y(I) * SH)
        YY = (X(I) * HS) + (Y(I) * CH)
        X(I) = XX
        Y(I) = YY
     END DO
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SH => TH
     DO I = 1, N
        XX = HFMA(Y(I), SH, X(I)) * CH
        YY = HFMA(X(I), HS, Y(I)) * CH
        X(I) = XX
        Y(I) = YY
     END DO
  END IF

  IF (LDA .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) RETURN
  IF (INFO .EQ. 0) THEN
     AX = ZERO
     DO J = 1, N
        DO I = 1, N
           X = CR_HYPOT(REAL(A(I,J)), AIMAG(A(I,J)))
           IF (.NOT. (X .LE. HUGE(X))) THEN
              INFO = -3
              RETURN
           END IF
           AX = MAX(AX, X)
        END DO
     END DO
  END IF
  ! upper bound
  IF (N .EQ. 1) THEN
     X = HUGE(X)
  ELSE ! N > 1
     X = N
     X = X * 9
     X = HUGE(X) / X
     X = SCALE(HALF, EXPONENT(X))
  END IF
  IF (AX .GT. X) THEN
     ! downscale (S < 0)
     S = EXPONENT(X) - EXPONENT(AX) - 1
     DO J = 1, N
        DO I = 1, N
           A(I,J) = CMPLX(SCALE(REAL(A(I,J)), S), SCALE(AIMAG(A(I,J)), S), K)
        END DO
     END DO
     AX = SCALE(AX, S)
     INFO = -S
  ELSE IF ((INFO .EQ. 0) .AND. (AX .LT. X)) THEN
     ! possibly upscale (S >= 0)
     S = EXPONENT(X) - EXPONENT(AX) - 1
     IF (S .GT. 0) THEN
        DO J = 1, N
           DO I = 1, N
              A(I,J) = CMPLX(SCALE(REAL(A(I,J)), S), SCALE(AIMAG(A(I,J)), S), K)
           END DO
        END DO
        AX = SCALE(AX, S)
        INFO = S
     END IF
  ELSE ! no-op
     S = 0
     INFO = 0
  END IF
  AS = AS + S

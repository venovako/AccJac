  IF (INFO .LT. 0) INFO = -(INFO + 1)
#ifndef NDEBUG
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  IF (N .EQ. 0) RETURN
  IF (INFO .EQ. 0) THEN
     GX = ZERO
     DO J = 1, N
        DO I = 1, M
           X = ABS(G(I,J))
#ifndef NDEBUG
           IF (.NOT. (X .LE. HUGE(X))) THEN
              INFO = -3
              RETURN
           END IF
#endif
           GX = MAX(GX, X)
        END DO
     END DO
  END IF
  ! upper bound
  S = 3
  IF (M .EQ. 1) THEN
     X = HUGE(X)
  ELSE ! M > 1
     X = M
     X = X * S
     X = HUGE(X) / X
     X = SCALE(HALF, EXPONENT(X))
  END IF
  IF (GX .GT. X) THEN
     ! downscale (S < 0)
     S = EXPONENT(X) - EXPONENT(GX) - 1
     DO J = 1, N
        DO I = 1, M
           G(I,J) = SCALE(G(I,J), S)
        END DO
     END DO
     GX = SCALE(GX, S)
     INFO = -S
  ELSE IF ((INFO .EQ. 0) .AND. (GX .LT. X)) THEN
     ! possibly upscale (S >= 0)
     S = EXPONENT(X) - EXPONENT(GX) - 1
     IF (S .GT. 0) THEN
        DO J = 1, N
           DO I = 1, M
              G(I,J) = SCALE(G(I,J), S)
           END DO
        END DO
        GX = SCALE(GX, S)
        INFO = S
     END IF
  ELSE ! no-op
     S = 0
     INFO = 0
  END IF
  GS = GS + S

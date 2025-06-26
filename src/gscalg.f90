  IF (INFO .LT. 0) INFO = -(INFO + 1)
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) RETURN
  IF (INFO .EQ. 0) THEN
     GX = ZERO
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,X) SHARED(G,M,N) REDUCTION(MAX:GX,INFO)
     DO J = 1, N
        DO I = 1, M
           X = ABS(G(I,J))
           IF (.NOT. (X .LE. HUGE(X))) INFO = MAX(INFO, ((J - 1) * N + I))
           GX = MAX(GX, X)
        END DO
     END DO
     !$OMP END PARALLEL DO
     IF (INFO .NE. 0) THEN
        INFO = -3
        RETURN
     END IF
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
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(G,M,N,S)
     DO J = 1, N
        DO I = 1, M
           G(I,J) = SCALE(G(I,J), S)
        END DO
     END DO
     !$OMP END PARALLEL DO
     GX = SCALE(GX, S)
     INFO = -S
  ELSE IF ((INFO .EQ. 0) .AND. (GX .LT. X)) THEN
     ! possibly upscale (S >= 0)
     S = EXPONENT(X) - EXPONENT(GX) - 1
     IF (S .GT. 0) THEN
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(G,M,N,S)
        DO J = 1, N
           DO I = 1, M
              G(I,J) = SCALE(G(I,J), S)
           END DO
        END DO
        !$OMP END PARALLEL DO
        GX = SCALE(GX, S)
        INFO = S
     END IF
  ELSE ! no-op
     S = 0
     INFO = 0
  END IF
  GS = GS + S

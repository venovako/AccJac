  ! INFO < 0 => GX is MAX(ABS(RE(g)),ABS(IM(g)))
  ! INFO >= 0 => GX is MAX(ABS(g))
  IF (INFO .LT. 0) THEN
     INFO = -(INFO + 1)
     S = 5
  ELSE ! INFO .GE. 0
     S = 3
  END IF
#ifndef NDEBUG
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  IF (N .EQ. 0) RETURN
  IF (INFO .EQ. 0) THEN
     GX = ZERO
     IF (S .EQ. 5) THEN
        DO J = 1, N
           DO I = 1, M
              X = ABS(REAL(G(I,J)))
#ifndef NDEBUG
              IF (.NOT. (X .LE. HUGE(X))) THEN
                 INFO = -3
                 RETURN
              END IF
#endif
              GX = MAX(GX, X)
              X = ABS(AIMAG(G(I,J)))
#ifndef NDEBUG
              IF (.NOT. (X .LE. HUGE(X))) THEN
                 INFO = -3
                 RETURN
              END IF
#endif
              GX = MAX(GX, X)
           END DO
        END DO
     ELSE ! S = 3
        DO J = 1, N
           DO I = 1, M
              X = CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
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
  END IF
  ! upper bound
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
           G(I,J) = CMPLX(SCALE(REAL(G(I,J)), S), SCALE(AIMAG(G(I,J)), S), K)
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
              G(I,J) = CMPLX(SCALE(REAL(G(I,J)), S), SCALE(AIMAG(G(I,J)), S), K)
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

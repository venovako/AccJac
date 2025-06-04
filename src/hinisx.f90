  INFO = 0
#ifndef NDEBUG
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  GX = ZERO
  IF (N .LE. 0) RETURN
  ! init V
  DO J = 1, N
     DO I = 1, J-1
        V(I,J) = ZERO
     END DO
     V(J,J) = ONE
     DO I = J+1, N
        V(I,J) = ZERO
     END DO
  END DO
  ! init IX
  DO J = 1, N
     IX(J) = J
  END DO
  ! init GX (with checking; see hscalg.f90 for the explanation)
  ! SV will hold the column norms later; for now, set it to something sensible
  DO J = 1, N
     SV(J) = ZERO
     DO I = 1, M
        Y = ABS(AIMAG(G(I,J)))
        IF (.NOT. (Y .LE. HUGE(Y))) THEN
           GX = J
           SV(J) = -I
           INFO = -3
           RETURN
        END IF
        X = ABS(REAL(G(I,J)))
        IF (.NOT. (X .LE. HUGE(X))) THEN
           GX = J
           SV(J) = I
           INFO = -3
           RETURN
        END IF
        SV(J) = MAX(SV(J), X, Y)
     END DO
     GX = MAX(GX, SV(J))
  END DO

#ifndef NDEBUG
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
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
  ! init SV
  IF (INFO .EQ. 0) THEN
#ifdef NDEBUG
     CONTINUE
#else
     DO J = 1, N
        SV(J) = ZERO
     END DO
#endif
  ELSE ! SLOW
     DO J = 1, N
        SV(J) = NRMF(M, G(1,J))
     END DO
  END IF
  INFO = 0

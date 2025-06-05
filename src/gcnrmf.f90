  INFO = 0
#ifndef NDEBUG
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  DO J = 1, N
     L = IX(J)
     SV(J) = NRMF(M, G(1,L))
#ifndef NDEBUG
     IF (.NOT. (SV(J) .LE. HUGE(SV(J)))) THEN
        INFO = J
        RETURN
     END IF
#endif
  END DO

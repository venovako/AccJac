  INFO = 0
#ifndef NDEBUG
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
#endif
  IF (M .LE. 0) THEN
     DO J = 1, N
        SV(J) = ZERO
     END DO
  ELSE ! M >= 1
     DO J = 1, N
        L = IX(J)
        SV(J) = ABS(G(1,L))
        DO I = 2, M
           SV(J) = CR_HYPOT(SV(J), G(I,L))
        END DO
#ifndef NDEBUG
        IF (.NOT. (SV(J) .LE. HUGE(ZERO))) THEN
           INFO = J
           RETURN
        END IF
#endif
     END DO
  END IF

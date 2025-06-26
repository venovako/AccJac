  INFO = 0
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -2
  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .LE. 1) RETURN
  I = 1
  L = 1
  DO WHILE (L .GT. 0)
     L = 0
     DO J = 1, JPOS-I
        IF (SV(J) .LT. SV(J+1)) THEN
           X = SV(J)
           SV(J) = SV(J+1)
           SV(J+1) = X
           M = IX(J)
           IX(J) = IX(J+1)
           IX(J+1) = M
           L = L + 1
        END IF
     END DO
     INFO = INFO + L
     I = I + 1
  END DO
  I = 1
  L = 1
  DO WHILE (L .GT. 0)
     L = 0
     DO J = JPOS+1, N-I
        IF (SV(J) .LT. SV(J+1)) THEN
           X = SV(J)
           SV(J) = SV(J+1)
           SV(J+1) = X
           M = IX(J)
           IX(J) = IX(J+1)
           IX(J+1) = M
           L = L + 1
        END IF
     END DO
     INFO = INFO + L
     I = I + 1
  END DO

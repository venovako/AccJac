  INFO = 0
  IF ((E .LE. 0) .OR. (E .GT. N)) INFO = -7
  IF ((B .LE. 0) .OR. (B .GT. E)) INFO = -6
  IF (LDV .LE. 0) INFO = -5
  IF (LDA .LE. 0) INFO = -3
  IF ((N .LE. 0) .OR. (N .GT. MIN(LDV, LDA))) INFO = -1
  IF (INFO .NE. 0) RETURN
  X = ZERO
  INFO = B
  DO J = B, E
     T = ABS(REAL(A(J,J)))
     IF (T .GT. X) THEN
        X = T
        INFO = J
     END IF
  END DO
  IF (INFO .GT. B) THEN
     DO J = 1, N
        Z = V(J,B)
        V(J,B) = V(J,INFO)
        V(J,INFO) = Z
     END DO
     CALL SWPC(N, A, LDA, B, INFO, J)
     IF (J .NE. 0) THEN
        INFO = -2
        RETURN
     END IF
     CALL SWPR(N, A, LDA, B, INFO, J)
     IF (J .NE. 0) THEN
        INFO = -4
        RETURN
     END IF
  END IF

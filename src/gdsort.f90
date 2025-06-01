  INFO = 0
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -6
  IF (LDV .LT. N) INFO = -5
  IF (LDA .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .LE. 1) RETURN
  I = 1
  L = 1
  DO WHILE (L .GT. 0)
     L = 0
     DO J = 1, JPOS-I
        IF (A(J,J) .LT. A(J+1,J+1)) THEN
           DO M = 1, N
              X = V(M,J)
              V(M,J) = V(M,J+1)
              V(M,J+1) = X
           END DO
           CALL SWPC(N, A, LDA, J, J+1, M)
           IF (M .NE. 0) THEN
              INFO = -2
              RETURN
           END IF
           CALL SWPR(N, A, LDA, J, J+1, M)
           IF (M .NE. 0) THEN
              INFO = -2
              RETURN
           END IF
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
        IF (A(J,J) .LT. A(J+1,J+1)) THEN
           DO M = 1, N
              X = V(M,J)
              V(M,J) = V(M,J+1)
              V(M,J+1) = X
           END DO
           CALL SWPC(N, A, LDA, J, J+1, M)
           IF (M .NE. 0) THEN
              INFO = -2
              RETURN
           END IF
           CALL SWPR(N, A, LDA, J, J+1, M)
           IF (M .NE. 0) THEN
              INFO = -2
              RETURN
           END IF
           L = L + 1
        END IF
     END DO
     INFO = INFO + L
     I = I + 1
  END DO

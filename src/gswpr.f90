  INFO = 0
  IF ((Q .LE. 0) .OR. (Q .GT. N)) INFO = -5
  IF ((P .LE. 0) .OR. (P .GT. N)) INFO = -4
  IF (LDA .LE. 0) INFO = -3
  IF ((N .LE. 0) .OR. (N .GT. LDA)) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (P .EQ. Q) RETURN
  DO J = 1, N
     T = A(P,J)
     A(P,J) = A(Q,J)
     A(Q,J) = T
  END DO

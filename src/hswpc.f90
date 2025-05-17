  INFO = 0
  IF ((Q .LE. 0) .OR. (Q .GT. N)) INFO = -5
  IF ((P .LE. 0) .OR. (P .GT. N)) INFO = -4
  IF (LDA .LE. 0) INFO = -3
  IF ((N .LE. 0) .OR. (N .GT. LDA)) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (P .EQ. Q) RETURN
  DO I = P, Q-1
     T = A(I,P)
     A(I,P) = CMPLX(REAL(A(Q,I)), -AIMAG(A(Q,I)), K)
     A(Q,I) = CMPLX(REAL(T), -AIMAG(T), K)
  END DO
  DO I = Q, N
     T = A(I,P)
     A(I,P) = A(I,Q)
     A(I,Q) = T
  END DO

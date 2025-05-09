PURE SUBROUTINE PASCAL(N, L, U, S, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
  INTEGER, INTENT(OUT) :: L(N,N), U(N,N), S(N,N), INFO
  INTEGER :: I, J
  INFO = 0
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN
  L(1,1) = 1
  DO I = 2, N
     L(I,1) = 1
     L(I,I) = 1
  END DO
  DO J = 2, N
     DO I = 1, J-1
        L(I,J) = 0
     END DO
     DO I = J+1, N
        L(I,J) = L(I-1,J-1) + L(I-1,J)
     END DO
  END DO
  U = TRANSPOSE(L)
  S = MATMUL(L, U)
END SUBROUTINE PASCAL

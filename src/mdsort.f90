PURE SUBROUTINE MDSORT(N, A, LDA, V, LDV, JPOS, INFO)
  USE MPFR_F
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE MSWPC(N, A, LDA, P, Q, INFO)
       USE MPFR_F
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       TYPE(MPFR_T), INTENT(INOUT) :: A(LDA,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE MSWPC
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE MSWPR(N, A, LDA, P, Q, INFO)
       USE MPFR_F
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       TYPE(MPFR_T), INTENT(INOUT) :: A(LDA,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE MSWPR
  END INTERFACE
  INTEGER, INTENT(IN) :: N, LDA, LDV, JPOS
  TYPE(MPFR_T), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
  INTEGER, INTENT(OUT) :: INFO
  TYPE(MPFR_T) :: X
  INTEGER :: I, J, L, M
  INFO = 0
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -6
  IF (LDV .LT. N) INFO = -5
  IF (LDA .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .LE. 1) RETURN
  CALL MPFR_INIT_M(X)
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
           CALL MSWPC(N, A, LDA, J, J+1, M)
           IF (M .NE. 0) THEN
              INFO = -2
              GOTO 9
           END IF
           CALL MSWPR(N, A, LDA, J, J+1, M)
           IF (M .NE. 0) THEN
              INFO = -2
              GOTO 9
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
           CALL MSWPC(N, A, LDA, J, J+1, M)
           IF (M .NE. 0) THEN
              INFO = -2
              GOTO 9
           END IF
           CALL MSWPR(N, A, LDA, J, J+1, M)
           IF (M .NE. 0) THEN
              INFO = -2
              GOTO 9
           END IF
           L = L + 1
        END IF
     END DO
     INFO = INFO + L
     I = I + 1
  END DO
9 CALL MPFR_CLEAR_M(X)
END SUBROUTINE MDSORT

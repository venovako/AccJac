!>@brief \b JSTEP returns a list R of P pivot pairs in the step T, decoded from the array O containing S steps, of a strategy J for a matrix of order N.
PURE SUBROUTINE JSTEP(J, N, S, T, P, O, R, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: J, N, S, T, P, O(2,*)
  INTEGER, INTENT(OUT) :: R(2,P), INFO
  INTEGER :: I, JJ, K, L, M

  INFO = 0
  IF (P .LT. 0) INFO = -5
  IF (T .LT. 1) INFO = -4
  IF (S .LT. 1) INFO = -3
  IF (N .LT. 0) INFO = -2
  IF (J .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN
  IF (P .EQ. 0) RETURN

  M = MOD(T - 1, S) + 1
  IF (J .LE. 4) THEN
     JJ = J
  ELSE ! a sequential-parallel ordering
     JJ = J - 3
  END IF

  SELECT CASE (JJ)
  CASE (0, 1)
     L = 1
     IF (P .GT. L) THEN
        INFO = -5
        RETURN
     END IF
     I = M
     K = 1
     R(1,K) = O(1,I)
     R(2,K) = O(2,I)
  CASE (2, 4)
     IF (MOD(N, 2) .NE. 0) THEN
        INFO = -2
        RETURN
     END IF
     L = N / 2
     IF (P .GT. L) THEN
        INFO = -5
        RETURN
     END IF
     I = (M - 1) * L + 1
     DO K = 1, P
        R(1,K) = O(1,I)
        R(2,K) = O(2,I)
        I = I + 1
     END DO
  CASE DEFAULT
     INFO = -1
  END SELECT
END SUBROUTINE JSTEP

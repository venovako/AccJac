PURE SUBROUTINE SINISV(M, N, G, LDG, V, LDV, JPOS, SV, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION SNRMF(M, X)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       REAL(KIND=REAL32), INTENT(IN) :: X(M)
       REAL(KIND=REAL32) :: SNRMF
     END FUNCTION SNRMF
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL32
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(OUT) :: V(LDV,N), SV(N)
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=K) :: W
  INTEGER :: I, J, L, T
  INFO = 0
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -7
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF ((N .LT. 0) .OR. (N .GT. M)) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN
  DO J = 1, N
     SV(J) = SNRMF(M, G(1,J))
     IF ((.NOT. (SV(J) .LE. HUGE(ZERO))) .OR. (SV(J) .LE. ZERO)) THEN
        INFO = -3
        RETURN
     END IF
  END DO
  IF (N .EQ. 1) THEN
     V(1,1) = ONE
     RETURN
  END IF
  ! sort G, SV
  DO I = 1, N
     V(I,2) = I
  END DO
  INFO = 0
  I = 1
  T = 1
  DO WHILE (T .GT. 0)
     T = 0
     DO J = 1, JPOS-I
        IF (SV(J) .LT. SV(J+1)) THEN
           W = SV(J)
           SV(J) = SV(J+1)
           SV(J+1) = W
           W = V(J,2)
           V(J,2) = V(J+1,2)
           V(J+1,2) = W
           T = T + 1
        END IF
     END DO
     INFO = INFO + T
     I = I + 1
  END DO
  I = 1
  T = 1
  DO WHILE (T .GT. 0)
     T = 0
     DO J = JPOS, N-I
        IF (SV(J) .GT. SV(J+1)) THEN
           W = SV(J)
           SV(J) = SV(J+1)
           SV(J+1) = W
           W = V(J,2)
           V(J,2) = V(J+1,2)
           V(J+1,2) = W
           T = T + 1
        END IF
     END DO
     INFO = INFO + T
     I = I + 1
  END DO
  DO I = 1, N
     V(I,1) = V(I,2)
  END DO
  DO J = 1, N
     L = INT(V(J,2))
     IF (J .LT. L) THEN
        ! swap the Jth and the Lth column
        DO I = 1, M
           W = G(I,J)
           G(I,J) = G(I,L)
           G(I,L) = W
        END DO
     ELSE IF (J .GT. L) THEN
        T = INT(V(L,2))
        ! swap the Jth and the Tth column
        DO I = 1, M
           W = G(I,J)
           G(I,J) = G(I,T)
           G(I,T) = W
        END DO
        V(J,2) = T
     END IF
  END DO
  ! init V
  DO J = 2, N
     L = INT(V(J,1))
     DO I = 1, L-1
        V(I,J) = ZERO
     END DO
     V(L,J) = ONE
     DO I = L+1, N
        V(I,J) = ZERO
     END DO
  END DO
  L = INT(V(1,1))
  DO I = 1, L-1
     V(I,1) = ZERO
  END DO
  V(L,1) = ONE
  DO I = L+1, N
     V(I,1) = ZERO
  END DO
END SUBROUTINE SINISV

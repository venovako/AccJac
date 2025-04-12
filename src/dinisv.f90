PURE SUBROUTINE DINISV(M, N, G, LDG, V, LDV, JPOS, SV, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION DNRMF(M, X)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       REAL(KIND=REAL64), INTENT(IN) :: X(M)
       REAL(KIND=REAL64) :: DNRMF
     END FUNCTION DNRMF
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(OUT) :: V(LDV,N), SV(N)
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=K) :: W
  INTEGER :: I, J, L
  REAL(KIND=K), ALLOCATABLE :: H(:,:)
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -7
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF ((N .LT. 0) .OR. (N .GT. M)) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) RETURN
  DO J = 1, N
     SV(J) = DNRMF(M, G(1,J))
     IF ((.NOT. (SV(J) .LE. HUGE(ZERO))) .OR. (SV(J) .LE. ZERO)) THEN
        INFO = -3
        RETURN
     END IF
  END DO
  ! sort G, SV
  DO I = 1, N
     V(I,1) = I
  END DO
  I = 1
  L = 1
  IF (INFO .NE. 0) THEN
     INFO = 0
     DO WHILE (L .GT. 0)
        L = 0
        DO J = JPOS+1, N-I
           IF (SV(J) .GT. SV(J+1)) THEN
              W = SV(J)
              SV(J) = SV(J+1)
              SV(J+1) = W
              W = V(J,1)
              V(J,1) = V(J+1,1)
              V(J+1,1) = W
              L = L + 1
           END IF
        END DO
        INFO = INFO + L
        I = I + 1
     END DO
  ELSE ! INFO = 0
     DO WHILE (L .GT. 0)
        L = 0
        DO J = JPOS+1, N-I
           IF (SV(J) .LT. SV(J+1)) THEN
              W = SV(J)
              SV(J) = SV(J+1)
              SV(J+1) = W
              W = V(J,1)
              V(J,1) = V(J+1,1)
              V(J+1,1) = W
              L = L + 1
           END IF
        END DO
        INFO = INFO + L
        I = I + 1
     END DO
  END IF
  I = 1
  L = 1
  DO WHILE (L .GT. 0)
     L = 0
     DO J = 1, JPOS-I
        IF (SV(J) .LT. SV(J+1)) THEN
           W = SV(J)
           SV(J) = SV(J+1)
           SV(J+1) = W
           W = V(J,1)
           V(J,1) = V(J+1,1)
           V(J+1,1) = W
           L = L + 1
        END IF
     END DO
     INFO = INFO + L
     I = I + 1
  END DO
  ! ugly but simple
  IF (INFO .GT. 0) THEN
     ALLOCATE(H(M,N))
     DO J = 1, N
        L = INT(V(J,1))
        DO I = 1, M
           H(I,J) = G(I,L)
        END DO
     END DO
     DO J = 1, N
        DO I = 1, M
           G(I,J) = H(I,J)
        END DO
     END DO
     DEALLOCATE(H)
  END IF
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
END SUBROUTINE DINISV

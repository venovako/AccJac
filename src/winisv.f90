PURE SUBROUTINE WINISV(M, N, G, LDG, V, LDV, JPOS, SV, WRK, INFO)
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION WNRMF(M, X)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       COMPLEX(KIND=10), INTENT(IN) :: X(M)
       REAL(KIND=10) :: WNRMF
     END FUNCTION WNRMF
  END INTERFACE
  INTEGER, PARAMETER :: K = 10
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N)
  COMPLEX(KIND=K), INTENT(OUT) :: V(LDV,N), WRK(M,N)
  REAL(KIND=K), INTENT(OUT) :: SV(N)
  INTEGER, INTENT(INOUT) :: INFO
  COMPLEX(KIND=K) :: Z
  REAL(KIND=K) :: W
  INTEGER :: I, J, L
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -7
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF ((N .LT. 0) .OR. (N .GT. M)) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) RETURN
  DO J = 1, N
     SV(J) = WNRMF(M, G(1,J))
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
              Z = V(J,1)
              V(J,1) = V(J+1,1)
              V(J+1,1) = Z
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
              Z = V(J,1)
              V(J,1) = V(J+1,1)
              V(J+1,1) = Z
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
           Z = V(J,1)
           V(J,1) = V(J+1,1)
           V(J+1,1) = Z
           L = L + 1
        END IF
     END DO
     INFO = INFO + L
     I = I + 1
  END DO
  ! ugly but simple
  IF (INFO .GT. 0) THEN
     DO J = 1, N
        L = INT(REAL(V(J,1)))
        DO I = 1, M
           WRK(I,J) = G(I,L)
        END DO
     END DO
     DO J = 1, N
        DO I = 1, M
           G(I,J) = WRK(I,J)
        END DO
     END DO
  END IF
  ! init V
  DO J = 2, N
     L = INT(REAL(V(J,1)))
     DO I = 1, L-1
        V(I,J) = ZERO
     END DO
     V(L,J) = ONE
     DO I = L+1, N
        V(I,J) = ZERO
     END DO
  END DO
  L = INT(REAL(V(1,1)))
  DO I = 1, L-1
     V(I,1) = ZERO
  END DO
  V(L,1) = ONE
  DO I = L+1, N
     V(I,1) = ZERO
  END DO
END SUBROUTINE WINISV

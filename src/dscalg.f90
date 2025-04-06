! the first time this routine is called, let GS = 0 and INFO = 1
! otherwise, set INFO = 0
SUBROUTINE DSCALG(M, N, G, LDG, GX, GS, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, HALF = 0.5_K
  INTEGER, INTENT(IN) :: M, N, LDG
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(OUT) :: GX
  INTEGER, INTENT(INOUT) :: GS, INFO
  REAL(KIND=K) :: X
  INTEGER :: I, J, S
  IF (LDG .LT. M) INFO = -4
  IF ((N .LT. 0) .OR. (N .GT. M)) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) RETURN
  GX = ZERO
  IF (INFO .GT. 0) THEN
     DO J = 1, N
        DO I = 1, M
           X = ABS(G(I,J))
           IF (.NOT. (X .LE. HUGE(X))) THEN
              INFO = -3
              RETURN
           END IF
           GX = MAX(GX, X)
        END DO
     END DO
  ELSE ! INFO = 0
     DO J = 1, N
        DO I = 1, M
           X = ABS(G(I,J))
           GX = MAX(GX, X)
        END DO
     END DO
  END IF
  ! upper bound
  IF (M .EQ. 1) THEN
     X = HUGE(X)
  ELSE ! M > 1
     X = 3 * M
     X = HUGE(X) / X
     X = SCALE(HALF, EXPONENT(X))
  END IF
  IF (GX .GT. X) THEN
     ! downscale (S < 0)
     S = EXPONENT(X) - EXPONENT(GX) - 1
     DO J = 1, N
        DO I = 1, M
           G(I,J) = SCALE(G(I,J), S)
        END DO
     END DO
     GX = SCALE(GX, S)
     INFO = -S
  ELSE IF ((INFO .GT. 0) .AND. (GX .LT. X)) THEN
     ! possibly upscale (S >= 0)
     S = EXPONENT(X) - EXPONENT(GX) - INFO
     IF (S .GT. 0) THEN
        DO J = 1, N
           DO I = 1, M
              G(I,J) = SCALE(G(I,J), S)
           END DO
        END DO
        GX = SCALE(GX, S)
     END IF
     INFO = 0
  ELSE ! no-op
     S = 0
     INFO = 0
  END IF
  GS = GS + S
END SUBROUTINE DSCALG

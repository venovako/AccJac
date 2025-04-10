!  IN: GS = max sweeps, INFO = 0 or 1 (sin => tan)
! OUT: GS: backscale SV by 2**-GS, INFO: #sweeps
SUBROUTINE DJSVDC(M, N, G, LDG, V, LDV, JPOS, SV, GS, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE DSCALG(M, N, G, LDG, GX, GS, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,N), GX
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE DSCALG
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE DINISV(M, N, G, LDG, V, LDV, JPOS, SV, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=REAL64), INTENT(OUT) :: V(LDV,N), SV(N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE DINISV
  END INTERFACE
  INTERFACE
     SUBROUTINE DTRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,N), V(LDV,N), SV(N), GX
       REAL(KIND=REAL64), INTENT(IN) :: TOL
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE DTRANS
  END INTERFACE
  INTERFACE
     SUBROUTINE DTRACK(N, SV, GX, GS, SWP, NTR)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, GS, SWP, NTR
       REAL(KIND=REAL64), INTENT(IN) :: SV(N), GX
     END SUBROUTINE DTRACK
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, EPS = EPSILON(EPS) / 2
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(OUT) :: V(LDV,N), SV(N)
  INTEGER, INTENT(INOUT) :: GS, INFO
  REAL(KIND=K) :: GX, TOL
  INTEGER :: P, Q, R, S, T, W
  IF ((INFO .LT. 0) .OR. (INFO .GT. 1)) INFO = -10
  IF (GS .LT. 0) INFO = -9
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -7
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF ((N .LT. 0) .OR. (N .GT. M)) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) RETURN
  S = GS
  ! prescale G
  GX = ZERO
  GS = 0
  R = 0
  CALL DSCALG(M, N, G, LDG, GX, GS, R)
  IF (R .LT. 0) THEN
     INFO = -3
     RETURN
  END IF
  ! init SV, V; sort SV, G
  CALL DINISV(M, N, G, LDG, V, LDV, JPOS, SV, R)
  IF (R .LT. 0) THEN
     INFO = -3
     RETURN
  END IF
  TOL = M
  TOL = SQRT(TOL) * EPS
  CALL DTRACK(N, SV, GX, GS, -R, S)
  DO R = 1, S
     T = 0
     ! row-cyclic
     DO P = 1, N-1
        DO Q = P+1, N
           W = IAND(INFO, 1)
           IF ((P .LE. JPOS) .AND. (Q .GT. JPOS)) THEN
              W = IOR(W, 2)
           ELSE IF (P .GT. JPOS) THEN
              W = IOR(W, 4)
           END IF
           CALL DTRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, W)
           SELECT CASE (W)
           CASE (0,1)
              CONTINUE
           CASE (2,3)
              T = T + 1
           CASE DEFAULT
              INFO = -5
              RETURN
           END SELECT
        END DO
     END DO
     CALL DTRACK(N, SV, GX, GS, R, T)
     IF (T .EQ. 0) EXIT
  END DO
  ! rescale U
  IF (R .LE. S) THEN
     DO Q = 1, N
        IF (.NOT. (SV(Q) .GT. ZERO)) THEN
           INFO = -8
           RETURN
        END IF
        IF (SV(Q) .NE. ONE) THEN
           DO P = 1, M
              G(P,Q) = G(P,Q) / SV(Q)
           END DO
        END IF
     END DO
  END IF
  INFO = R
END SUBROUTINE DJSVDC

!  IN: GS = max sweeps, INFO = 0 or 1 (sin => tan) OR 2 (dsc/asc)
! OUT: GS: backscale SV by 2**-GS, INFO: #sweeps
SUBROUTINE XJSVDC(M, N, G, LDG, V, LDV, JPOS, SV, WRK, GS, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE XSCALG(M, N, G, LDG, GX, GS, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: G(LDG,N), GX
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: G(LDG,N), GX
#endif
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE XSCALG
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XINISV(M, N, G, LDG, V, LDV, JPOS, SV, WRK, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=c_long_double), INTENT(OUT) :: V(LDV,N), SV(N), WRK(M,N)
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=REAL128), INTENT(OUT) :: V(LDV,N), SV(N), WRK(M,N)
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XINISV
  END INTERFACE
  INTERFACE
     SUBROUTINE XTRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: G(LDG,N), V(LDV,N), SV(N), GX
       REAL(KIND=c_long_double), INTENT(IN) :: TOL
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: G(LDG,N), V(LDV,N), SV(N), GX
       REAL(KIND=REAL128), INTENT(IN) :: TOL
#endif
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE XTRANS
  END INTERFACE
  INTERFACE
     SUBROUTINE XTRACK(N, SV, GX, GS, SWP, NTR)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, GS, SWP, NTR
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: SV(N), GX
#else
       REAL(KIND=REAL128), INTENT(IN) :: SV(N), GX
#endif
     END SUBROUTINE XTRACK
  END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, EPS = EPSILON(EPS) / 2
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(OUT) :: V(LDV,N), SV(N), WRK(M,N)
  INTEGER, INTENT(INOUT) :: GS, INFO
  REAL(KIND=K) :: GX, TOL
  INTEGER :: P, Q, R, S, T, W
  IF ((INFO .LT. 0) .OR. (INFO .GT. 7)) INFO = -11
  IF (GS .LT. 0) INFO = -10
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
  CALL XSCALG(M, N, G, LDG, GX, GS, R)
  IF (R .LT. 0) THEN
     INFO = -3
     RETURN
  END IF
  ! init SV, V; sort SV, G
  R = IAND(INFO, 2)
  CALL XINISV(M, N, G, LDG, V, LDV, JPOS, SV, WRK, R)
  IF (R .LT. 0) THEN
     INFO = -9
     RETURN
  END IF
  TOL = M
  TOL = SQRT(TOL) * EPS
  CALL XTRACK(N, SV, GX, GS, -R, S)
  DO R = 1, S
     T = 0
     ! row-cyclic
     DO P = 1, N-1
        DO Q = P+1, N
           W = IAND(INFO, 1)
           IF ((P .LE. JPOS) .AND. (Q .GT. JPOS)) W = IOR(W, 2)
           IF ((P .GT. JPOS) .AND. (IAND(INFO, 2) .NE. 0)) THEN
              CALL XTRANS(M, N, G, LDG, V, LDV, SV, GX, GS, Q, P, TOL, W)
           ELSE ! not asc
              CALL XTRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, W)
           END IF
           SELECT CASE (W)
           CASE (0)
              CONTINUE
           CASE (1,2,3)
              T = T + 1
           CASE DEFAULT
              INFO = -5
              RETURN
           END SELECT
        END DO
     END DO
     CALL XTRACK(N, SV, GX, GS, R, T)
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
END SUBROUTINE XJSVDC

!  IN: GS = max sweeps, IX(1) = trace unit, INFO
! OUT: GS: backscale SV by 2**-GS, INFO: #sweeps
SUBROUTINE WJSVDF(M, N, G, LDG, V, LDV, JPOS, SV, GS, IX, WRK, RWRK, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL128
#endif
  IMPLICIT NONE
#include "cr.f90"
  INTERFACE
#ifdef _OPENMP
     SUBROUTINE WINISX(M, N, G, LDG, V, LDV, SV, IX, INFO)
#else
     PURE SUBROUTINE WINISX(M, N, G, LDG, V, LDV, SV, IX, INFO)
#endif
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(IN) :: G(LDG,N)
       COMPLEX(KIND=c_long_double), INTENT(OUT) :: V(LDV,N)
       REAL(KIND=c_long_double), INTENT(OUT) :: SV(N)
#else
       COMPLEX(KIND=REAL128), INTENT(IN) :: G(LDG,N)
       COMPLEX(KIND=REAL128), INTENT(OUT) :: V(LDV,N)
       REAL(KIND=REAL128), INTENT(OUT) :: SV(N)
#endif
       INTEGER, INTENT(OUT) :: IX(N)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WINISX
  END INTERFACE
  INTERFACE
#ifdef _OPENMP
     SUBROUTINE WSCALG(M, N, G, LDG, GX, GS, INFO)
#else
     PURE SUBROUTINE WSCALG(M, N, G, LDG, GX, GS, INFO)
#endif
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: GX
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=REAL128), INTENT(INOUT) :: GX
#endif
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE WSCALG
  END INTERFACE
  INTERFACE
#ifdef _OPENMP
     SUBROUTINE WPRCYC(M, N, G, LDG, JPOS, SV, IX, WRK, RWRK, INFO)
#else
     PURE SUBROUTINE WPRCYC(M, N, G, LDG, JPOS, SV, IX, WRK, RWRK, INFO)
#endif
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, JPOS
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: G(LDG,N)
       COMPLEX(KIND=c_long_double), INTENT(OUT) :: WRK(M,N)
       REAL(KIND=c_long_double), INTENT(OUT) :: SV(N), RWRK(N)
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: G(LDG,N)
       COMPLEX(KIND=REAL128), INTENT(OUT) :: WRK(M,N)
       REAL(KIND=REAL128), INTENT(OUT) :: SV(N), RWRK(N)
#endif
       INTEGER, INTENT(INOUT) :: IX(N), INFO
     END SUBROUTINE WPRCYC
  END INTERFACE
  INTERFACE
     SUBROUTINE WTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, IX, WRK, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q, IX(N)
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: G(LDG,N), V(LDV,N), TOL
       COMPLEX(KIND=c_long_double), INTENT(OUT) :: WRK(M,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: SV(N), GX
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: G(LDG,N), V(LDV,N), TOL
       COMPLEX(KIND=REAL128), INTENT(OUT) :: WRK(M,N)
       REAL(KIND=REAL128), INTENT(INOUT) :: SV(N), GX
#endif
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE WTRNSF
  END INTERFACE
  INTERFACE
     SUBROUTINE WTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, IX, WRK, RWRK, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q, IX(N)
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: G(LDG,N), V(LDV,N), TOL, WRK(M,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: SV(N), GX, RWRK(N)
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: G(LDG,N), V(LDV,N), TOL, WRK(M,N)
       REAL(KIND=REAL128), INTENT(INOUT) :: SV(N), GX, RWRK(N)
#endif
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE WTRUTI
  END INTERFACE
  INTERFACE
     SUBROUTINE XTRACK(N, SV, GX, GS, SWP, NTR, U)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, GS, SWP, NTR, U
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
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N)
  COMPLEX(KIND=K), INTENT(OUT) :: V(LDV,N), WRK(M,N)
  REAL(KIND=K), INTENT(OUT) :: SV(N), RWRK(N)
  INTEGER, INTENT(INOUT) :: GS, IX(N), INFO
  COMPLEX(KIND=K) :: Z
  REAL(KIND=K) :: GX, TOL, X
  INTEGER(KIND=INT64) :: TT
  INTEGER :: L, O, P, Q, R, S, T, U, W
  IF ((INFO .LT. 0) .OR. (INFO .GT. 7)) INFO = -13
  IF (GS .LT. 0) INFO = -9
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -7
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. N) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) THEN
     INFO = 0
     RETURN
  END IF
  S = GS
  GS = 0
  TT = 0_INT64
  L = ISHFT(IAND(INFO, 6), -1)
  INFO = IAND(INFO, 1)
  U = IX(1)
  O = -1
  CALL WSCALG(M, N, G, LDG, GX, GS, O)
  IF (O .LT. 0) THEN
     INFO = -3
     GOTO 9
  END IF
  R = INFO
  CALL WINISX(M, N, G, LDG, V, LDV, SV, IX, R)
  IF (R .NE. 0) THEN
     INFO = -10
     GOTO 9
  END IF
  CALL XTRACK(N, SV, GX, GS, R, -S, U)
  TOL = M
  TOL = CR_SQRT(TOL) * EPS
  DO R = 1, S
     IF (INFO .EQ. 0) THEN
        O = 1
     ELSE ! SLOW
        O = 0
     END IF
     IF ((L .EQ. 2) .OR. (L .EQ. 3)) O = 2
     CALL WPRCYC(M, N, G, LDG, JPOS, SV, IX, WRK, RWRK, O)
     IF (O .LT. 0) THEN
        INFO = -8
        GOTO 9
     END IF
     T = 0
     IF (IAND(L, 1) .EQ. 0) THEN
        ! the first diagonal block
        DO P = 1, JPOS-1
           IF (P .GT. 1) THEN
              X = ZERO
              W = 0
              DO Q = P, JPOS
                 IF (SV(Q) .GT. X) THEN
                    X = SV(Q)
                    W = Q
                 END IF
              END DO
              IF (W .GT. P) THEN
                 X = SV(P)
                 SV(P) = SV(W)
                 SV(W) = X
                 O = IX(P)
                 IX(P) = IX(W)
                 IX(W) = O
                 T = T + 1
              END IF
           END IF
           DO Q = P+1, JPOS
              Z = CMPLX(TOL, REAL(R, K), K)
              IF (INFO .EQ. 0) THEN
                 O = 0
              ELSE ! SLOW
                 O = 2
              END IF
              IF (L .EQ. 0) THEN
                 CALL WTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, Z, IX, WRK, O)
              ELSE ! L = 2
                 CALL WTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, Z, IX, WRK, RWRK, O)
              END IF
              SELECT CASE (O)
              CASE (0,1)
                 CONTINUE
              CASE (4,5)
                 T = T + 1
              CASE (2,3,6,7)
                 T = T + 1
                 TT = TT + 1_INT64
              CASE DEFAULT
                 INFO = -5
                 GOTO 9
              END SELECT
           END DO
        END DO
        ! the off-diagonal block (hyp)
        DO P = 1, JPOS
           DO Q = JPOS+1, N
              Z = CMPLX(TOL, REAL(R, K), K)
              IF (INFO .EQ. 0) THEN
                 O = 1
              ELSE ! SLOW
                 O = 3
              END IF
              IF (L .EQ. 0) THEN
                 CALL WTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, Z, IX, WRK, O)
              ELSE ! L = 2
                 CALL WTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, Z, IX, WRK, RWRK, O)
              END IF
              SELECT CASE (O)
              CASE (0,1)
                 CONTINUE
              CASE (4,5)
                 T = T + 1
              CASE (2,3,6,7)
                 T = T + 1
                 TT = TT + 1_INT64
              CASE DEFAULT
                 INFO = -5
                 GOTO 9
              END SELECT
              RWRK(N) = MAX(RWRK(N), ABS(REAL(Z)))
           END DO
        END DO
        ! the second diagonal block
        DO P = JPOS+1, N-1
           X = ZERO
           W = 0
           DO Q = P, N
              IF (SV(Q) .GT. X) THEN
                 X = SV(Q)
                 W = Q
              END IF
           END DO
           IF (W .GT. P) THEN
              X = SV(P)
              SV(P) = SV(W)
              SV(W) = X
              O = IX(P)
              IX(P) = IX(W)
              IX(W) = O
              T = T + 1
           END IF
           DO Q = P+1, N
              Z = CMPLX(TOL, REAL(R, K), K)
              IF (INFO .EQ. 0) THEN
                 O = 0
              ELSE ! SLOW
                 O = 2
              END IF
              IF (L .EQ. 0) THEN
                 CALL WTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, Z, IX, WRK, O)
              ELSE ! L = 2
                 CALL WTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, Z, IX, WRK, RWRK, O)
              END IF
              SELECT CASE (O)
              CASE (0,1)
                 CONTINUE
              CASE (4,5)
                 T = T + 1
              CASE (2,3,6,7)
                 T = T + 1
                 TT = TT + 1_INT64
              CASE DEFAULT
                 INFO = -5
                 GOTO 9
              END SELECT
           END DO
        END DO
     ELSE ! row-cyclic
        DO P = 1, N-1
           DO Q = P+1, N
              Z = CMPLX(TOL, REAL(R, K), K)
              IF ((P .LE. JPOS) .AND. (Q .GT. JPOS)) THEN
                 IF (INFO .EQ. 0) THEN
                    O = 1
                 ELSE ! SLOW
                    O = 3
                 END IF
              ELSE ! trig
                 IF (INFO .EQ. 0) THEN
                    O = 0
                 ELSE ! SLOW
                    O = 2
                 END IF
              END IF
              IF (L .EQ. 1) THEN
                 CALL WTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, Z, IX, WRK, O)
              ELSE ! L = 3
                 CALL WTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, Z, IX, WRK, RWRK, O)
              END IF
              SELECT CASE (O)
              CASE (0,1)
                 CONTINUE
              CASE (4,5)
                 T = T + 1
              CASE (2,3,6,7)
                 T = T + 1
                 TT = TT + 1_INT64
              CASE DEFAULT
                 INFO = -5
                 GOTO 9
              END SELECT
              IF ((P .LE. JPOS) .AND. (Q .GT. JPOS)) RWRK(N) = MAX(RWRK(N), ABS(REAL(Z)))
           END DO
        END DO
     END IF
     CALL XTRACK(N, SV, RWRK(N), GS, R, -T, U)
     IF (T .EQ. 0) EXIT
  END DO
  IF (R .LE. S) THEN
     ! permute V
     DO Q = 1, N
        DO P = 1, N
           WRK(P,Q) = V(P,IX(Q))
        END DO
     END DO
     DO Q = 1, N
        DO P = 1, N
           V(P,Q) = WRK(P,Q)
        END DO
     END DO
     ! permute and rescale U
     DO Q = 1, N
        IF (.NOT. (SV(Q) .GT. ZERO)) THEN
           INFO = -11
           GOTO 9
        END IF
        IF (SV(Q) .NE. ONE) THEN
           IF (INFO .EQ. 0) THEN
              X = ONE / SV(Q)
              DO P = 1, M
                 Z = G(P,IX(Q))
                 WRK(P,Q) = CMPLX(REAL(Z) * X, AIMAG(Z) * X, K)
              END DO
           ELSE ! SLOW
              DO P = 1, M
                 Z = G(P,IX(Q))
                 WRK(P,Q) = CMPLX(REAL(Z) / SV(Q), AIMAG(Z) / SV(Q), K)
              END DO
           END IF
        ELSE ! no division
           DO P = 1, M
              WRK(P,Q) = G(P,IX(Q))
           END DO
        END IF
     END DO
     DO Q = 1, N
        DO P = 1, M
           G(P,Q) = WRK(P,Q)
        END DO
     END DO
  END IF
  INFO = R
9 RWRK(N) = REAL(TT, K)
END SUBROUTINE WJSVDF

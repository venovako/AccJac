!  IN: GS = max sweeps, IX(1) = trace unit, INFO
! OUT: GS: backscale SV by 2**-GS, INFO: #sweeps
SUBROUTINE DJSVDP(M, N, G, LDG, V, LDV, JPOS, SV, GS, IX, WRK, RWRK, TBL, ORD, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL64
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE JSTEP(J, N, S, T, P, O, R, INFO)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: J, N, S, T, P, O(2,*)
       INTEGER, INTENT(OUT) :: R(2,P), INFO
     END SUBROUTINE JSTEP
  END INTERFACE
  INTERFACE
#ifdef _OPENMP
     SUBROUTINE DINISX(M, N, G, LDG, V, LDV, SV, IX, INFO)
#else
     PURE SUBROUTINE DINISX(M, N, G, LDG, V, LDV, SV, IX, INFO)
#endif
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV
       REAL(KIND=REAL64), INTENT(IN) :: G(LDG,N)
       REAL(KIND=REAL64), INTENT(OUT) :: V(LDV,N), SV(N)
       INTEGER, INTENT(OUT) :: IX(N)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DINISX
  END INTERFACE
  INTERFACE
#ifdef _OPENMP
     SUBROUTINE DSCALG(M, N, G, LDG, GX, GS, INFO)
#else
     PURE SUBROUTINE DSCALG(M, N, G, LDG, GX, GS, INFO)
#endif
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,N), GX
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE DSCALG
  END INTERFACE
  INTERFACE
#ifdef _OPENMP
     SUBROUTINE DPRCYC(M, N, G, LDG, JPOS, SV, IX, WRK, RWRK, INFO)
#else
     PURE SUBROUTINE DPRCYC(M, N, G, LDG, JPOS, SV, IX, WRK, RWRK, INFO)
#endif
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, JPOS
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=REAL64), INTENT(OUT) :: SV(N), WRK(M,N), RWRK(N)
       INTEGER, INTENT(INOUT) :: IX(N), INFO
     END SUBROUTINE DPRCYC
  END INTERFACE
  INTERFACE
     SUBROUTINE DTRNSP(M, N, G, LDG, V, LDV, SV, GX, P, Q, TOL, IX, WRK, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q, IX(N)
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,N), V(LDV,N), SV(N), GX, TOL
       REAL(KIND=REAL64), INTENT(OUT) :: WRK(M,N)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DTRNSP
  END INTERFACE
  INTERFACE
     SUBROUTINE DTRACK(N, SV, GX, GS, SWP, NTR, U)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, GS, SWP, NTR, U
       REAL(KIND=REAL64), INTENT(IN) :: SV(N), GX
     END SUBROUTINE DTRACK
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, EPS = EPSILON(EPS) / 2
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS, TBL(2,*)
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(OUT) :: V(LDV,N), SV(N), WRK(M,N), RWRK(N)
  INTEGER, INTENT(INOUT) :: GS, IX(N), ORD(2,*), INFO
  REAL(KIND=K) :: GX, TOL, X, Y, Z
  INTEGER(KIND=INT64) :: TT
  INTEGER :: L, O, P, Q, R, S, T, U, W, ST, TP, TS
#define GINISX DINISX
#define GSCALG DSCALG
#define GPRCYC DPRCYC
#define GTRNSP DTRNSP
#define GTRACK DTRACK
#define GSQRT SQRT
#include "gjsvdp.f90"
END SUBROUTINE DJSVDP

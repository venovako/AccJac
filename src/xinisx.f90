#ifdef _OPENMP
SUBROUTINE XINISX(M, N, G, LDG, V, LDV, SV, IX, INFO)
#else
PURE SUBROUTINE XINISX(M, N, G, LDG, V, LDV, SV, IX, INFO)
#endif
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION XNRMF(M, X)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: X(M)
       REAL(KIND=c_long_double) :: XNRMF
#else
       REAL(KIND=REAL128), INTENT(IN) :: X(M)
       REAL(KIND=REAL128) :: XNRMF
#endif
     END FUNCTION XNRMF
  END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, LDV
  REAL(KIND=K), INTENT(IN) :: G(LDG,N)
  REAL(KIND=K), INTENT(OUT) :: V(LDV,N), SV(N)
  INTEGER, INTENT(OUT) :: IX(N)
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER :: I, J
#define NRMF XNRMF
#include "ginisx.f90"
END SUBROUTINE XINISX

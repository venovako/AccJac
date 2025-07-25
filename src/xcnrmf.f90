#ifdef _OPENMP
SUBROUTINE XCNRMF(M, N, G, LDG, SV, IX, INFO)
#else
PURE SUBROUTINE XCNRMF(M, N, G, LDG, SV, IX, INFO)
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
  INTEGER, INTENT(IN) :: M, N, LDG, IX(N)
  REAL(KIND=K), INTENT(IN) :: G(LDG,N)
  REAL(KIND=K), INTENT(OUT) :: SV(N)
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: J, L
#define NRMF XNRMF
#include "gcnrmf.f90"
END SUBROUTINE XCNRMF

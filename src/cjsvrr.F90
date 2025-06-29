PROGRAM CJSVRR
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, REAL32
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, REAL32, REAL128
#endif
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  IMPLICIT NONE
#ifdef USE_IEEE_INTRINSIC
#define XFMA IEEE_FMA
#else
  INTERFACE
#ifdef __GFORTRAN__
     PURE FUNCTION XFMA(A, B, C) BIND(C,NAME='fmal')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
     PURE FUNCTION XFMA(A, B, C) BIND(C,NAME='__fmaq')
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: A, B, C
       REAL(KIND=c_long_double) :: XFMA
#else
       REAL(KIND=REAL128), INTENT(IN), VALUE :: A, B, C
       REAL(KIND=REAL128) :: XFMA
#endif
     END FUNCTION XFMA
  END INTERFACE
#endif
#ifdef __GFORTRAN__
#define WFMA(A,B,C) CMPLX(XFMA(REAL(A),REAL(B),XFMA(-AIMAG(A),AIMAG(B),REAL(C))),XFMA(REAL(A),AIMAG(B),XFMA(AIMAG(A),REAL(B),AIMAG(C))),c_long_double)
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  INTEGER, PARAMETER :: KK = c_long_double
#else
#define WFMA(A,B,C) CMPLX(XFMA(REAL(A),REAL(B),XFMA(-AIMAG(A),AIMAG(B),REAL(C))),XFMA(REAL(A),AIMAG(B),XFMA(AIMAG(A),REAL(B),AIMAG(C))),REAL128)
#define CR_HYPOT HYPOT
  INTEGER, PARAMETER :: KK = REAL128
#endif
  INTEGER, PARAMETER :: K = REAL32
  REAL(KIND=KK), PARAMETER :: XZERO = 0.0_KK
  CHARACTER(LEN=256) :: CLA
  COMPLEX(KIND=K), ALLOCATABLE :: G(:,:), V(:,:), U(:,:)
  REAL(KIND=K), ALLOCATABLE :: S(:)
  COMPLEX(KIND=KK), ALLOCATABLE :: XG(:,:), XV(:,:), XU(:,:)
  REAL(KIND=KK), ALLOCATABLE :: XS(:)
  REAL(KIND=KK) :: X, GN
  INTEGER :: M, N, I, J, L
  EXTERNAL :: BFOPEN
  !$OMP DECLARE REDUCTION(HYP:REAL(KK):OMP_OUT=CR_HYPOT(OMP_OUT,OMP_IN)) INITIALIZER(OMP_PRIV=XZERO)
#include "hjsvrr.F90"
9 FORMAT(I11,A,ES16.9E2)
END PROGRAM CJSVRR

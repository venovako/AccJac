SUBROUTINE WLJAV2(A11, A22, A21R, A21I, CH, SHR, SHI, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int, c_long_double
#else
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
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
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  INTERFACE
     FUNCTION PVN_WLJV2(A11, A22, A21R, A21I, CH, SHR, SHI, TH, ES)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int, c_long_double
       REAL(KIND=c_long_double), INTENT(IN) :: A11, A22, A21R, A21I
       REAL(KIND=c_long_double), INTENT(OUT) :: CH, SHR, SHI
       REAL(KIND=c_long_double), INTENT(INOUT) :: TH
       INTEGER(KIND=c_int), INTENT(INOUT) :: ES
       INTEGER(KIND=c_int) :: PVN_WLJV2
     END FUNCTION PVN_WLJV2
  END INTERFACE
  INTEGER, PARAMETER :: K = c_long_double
#else
#define CR_HYPOT HYPOT
  INTERFACE
     FUNCTION PVN_YLJV2(A11, A22, A21R, A21I, CH, SHR, SHI, TH, ES)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       REAL(KIND=REAL128), INTENT(IN) :: A11, A22, A21R, A21I
       REAL(KIND=REAL128), INTENT(OUT) :: CH, SHR, SHI
       REAL(KIND=REAL128), INTENT(INOUT) :: TH
       INTEGER(KIND=c_int), INTENT(INOUT) :: ES
       INTEGER(KIND=c_int) :: PVN_YLJV2
     END FUNCTION PVN_YLJV2
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  REAL(KIND=K), INTENT(INOUT) :: A11, A22, CH
  REAL(KIND=K), INTENT(IN) :: A21R, A21I
  REAL(KIND=K), INTENT(OUT) :: SHR, SHI
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=K) :: A, TH
  INTEGER(KIND=c_int) :: ES, RT
#define GFMA XFMA
#ifdef __GFORTRAN__
#define LJV2 PVN_WLJV2
#else
#define LJV2 PVN_YLJV2
#endif
#include "hljav2.f90"
END SUBROUTINE WLJAV2

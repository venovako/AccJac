SUBROUTINE ZLJAV2(A11, A22, A21R, A21I, CH, SHR, SHI, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION DFMA(A, B, C)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       REAL(KIND=REAL64), INTENT(IN) :: A, B, C
       REAL(KIND=REAL64) :: DFMA
     END FUNCTION DFMA
  END INTERFACE
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       IMPLICIT NONE
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  INTERFACE
     FUNCTION PVN_ZLJV2(A11, A22, A21R, A21I, CH, SHR, SHI, TH, ES)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int, c_double
       REAL(KIND=c_double), INTENT(IN) :: A11, A22, A21R, A21I
       REAL(KIND=c_double), INTENT(OUT) :: CH, SHR, SHI, TH
       INTEGER(KIND=c_int), INTENT(INOUT) :: ES
       INTEGER(KIND=c_int) :: PVN_ZLJV2
     END FUNCTION PVN_ZLJV2
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  REAL(KIND=K), INTENT(INOUT) :: A11, A22
  REAL(KIND=K), INTENT(IN) :: A21R, A21I
  REAL(KIND=K), INTENT(OUT) :: CH, SHR, SHI
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=K) :: A, TH
  INTEGER(KIND=c_int) :: ES, RT
#define GFMA DFMA
#define LJV2 PVN_ZLJV2
#include "hljav2.f90"
END SUBROUTINE ZLJAV2

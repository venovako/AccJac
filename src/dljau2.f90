SUBROUTINE DLJAU2(A11, A22, A21, CS, SN, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTERFACE
     FUNCTION PVN_DLJEV2(A11, A22, A21, CS, SN, L1, L2, ES)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int, c_double
       REAL(KIND=c_double), INTENT(IN) :: A11, A22, A21
       REAL(KIND=c_double), INTENT(OUT) :: CS, SN, L1, L2
       INTEGER(KIND=c_int), INTENT(INOUT) :: ES
       INTEGER(KIND=c_int) :: PVN_DLJEV2
     END FUNCTION PVN_DLJEV2
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  REAL(KIND=K), INTENT(INOUT) :: A11, A22
  REAL(KIND=K), INTENT(IN) :: A21
  REAL(KIND=K), INTENT(OUT) :: CS, SN
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=K) :: L1, L2
  INTEGER :: S
  INTEGER(KIND=c_int) :: ES, RT
#define LJU2 PVN_DLJEV2
#include "gljau2.f90"
END SUBROUTINE DLJAU2

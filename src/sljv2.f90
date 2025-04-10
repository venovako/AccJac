SUBROUTINE SLJV2(A11, A22, A21, CH, SH, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTERFACE
     FUNCTION PVN_LJV2(A11, A22, A21, CH, SH, ES) BIND(C,NAME='pvn_sljv2_')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int, c_float
       REAL(KIND=c_float), INTENT(IN) :: A11, A22, A21
       REAL(KIND=c_float), INTENT(OUT) :: CH, SH
       INTEGER(KIND=c_int), INTENT(INOUT) :: ES
       INTEGER(KIND=c_int) :: PVN_LJV2
     END FUNCTION PVN_LJV2
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL32
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  REAL(KIND=K), PARAMETER :: CUTTH = 4.0_K / 5.0_K
  REAL(KIND=K), PARAMETER :: CUTCH = 5.0_K / 3.0_K
  REAL(KIND=K), PARAMETER :: CUTSH = 4.0_K / 3.0_K
  REAL(KIND=K), INTENT(IN) :: A11, A22, A21
  REAL(KIND=K), INTENT(OUT) :: CH, SH
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=K) :: A
  INTEGER(KIND=c_int) :: ES, RT
  INCLUDE 'gljv2.f90'
END SUBROUTINE SLJV2

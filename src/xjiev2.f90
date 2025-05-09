SUBROUTINE XJIEV2(A, B, C, RT1, RT2, CS1, SN1, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int, c_long_double
  IMPLICIT NONE

  REAL(c_long_double), PARAMETER :: ZERO = 0.0_c_long_double, ONE = 1.0_c_long_double
  REAL(c_long_double), INTENT(IN) :: A, B, C
  REAL(c_long_double), INTENT(OUT) :: RT1, RT2, CS1, SN1
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER(c_int) :: ES
  INTEGER(c_int), EXTERNAL :: PVN_XLJEV2

  ES = INT(INFO, c_int)
  INFO = INT(PVN_XLJEV2(A, C, B, CS1, SN1, RT1, RT2, ES))
  IF (INFO .LT. 0) THEN
     CS1 = ONE
     SN1 = ZERO
     RT1 = ZERO
     RT2 = ZERO
  ELSE IF (ES .NE. 0_c_int) THEN
     RT1 = SCALE(RT1, ES)
     RT2 = SCALE(RT2, ES)
  END IF
END SUBROUTINE XJIEV2

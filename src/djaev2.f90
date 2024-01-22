SUBROUTINE DJAEV2(A, B, C, RT1, RT2, CS1, SN1)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double, c_int
  IMPLICIT NONE

  REAL(c_double), PARAMETER :: ZERO = 0.0_c_double, ONE = 1.0_c_double
  REAL(c_double), INTENT(IN) :: A, B, C
  REAL(c_double), INTENT(OUT) :: RT1, RT2, CS1, SN1
  INTEGER(c_int) :: ES
  INTEGER(c_int), EXTERNAL :: PVN_DLJEV2

  ES = 0_c_int
  IF (PVN_DLJEV2(A, C, B, CS1, SN1, RT1, RT2, ES) .LT. 0_c_int) THEN
     CS1 = ONE
     SN1 = ZERO
     RT1 = ZERO
     RT2 = ZERO
  ELSE IF (ES .NE. 0_c_int) THEN
     RT1 = SCALE(RT1, ES)
     RT2 = SCALE(RT2, ES)
  END IF
END SUBROUTINE DJAEV2

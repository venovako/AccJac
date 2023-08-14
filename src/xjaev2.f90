SUBROUTINE XJAEV2(A, B, C, RT1, RT2, CS1, SN1)
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  INTERFACE
     FUNCTION XLJEV2(A11, A22, A21, CS, SN, L1, L2, ES) BIND(C,NAME='pvn_xljev2_')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       REAL(c_long_double), INTENT(IN), TARGET :: A11, A22, A21
       REAL(c_long_double), INTENT(OUT), TARGET :: CS, SN, L1, L2
       INTEGER(c_int), INTENT(INOUT), TARGET :: ES
       INTEGER(c_int) :: XLJEV2
     END FUNCTION XLJEV2
  END INTERFACE

  REAL(c_long_double), PARAMETER :: ZERO = 0.0_c_long_double, ONE = 1.0_c_long_double
  REAL(c_long_double), INTENT(IN), TARGET :: A, B, C
  REAL(c_long_double), INTENT(OUT), TARGET :: RT1, RT2, CS1, SN1
  INTEGER(c_int), TARGET :: ES

  ES = 0_c_int
  IF (XLJEV2(A, C, B, CS1, SN1, RT1, RT2, ES) .LT. 0_c_int) THEN
     CS1 = ONE
     SN1 = ZERO
     RT1 = ZERO
     RT2 = ZERO
  ELSE IF (ES .NE. 0_c_int) THEN
     RT1 = SCALE(RT1, ES)
     RT2 = SCALE(RT2, ES)
  END IF
END SUBROUTINE XJAEV2

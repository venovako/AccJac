SUBROUTINE SJIEV2(A, B, C, RT1, RT2, CS1, SN1, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float, c_int
  IMPLICIT NONE

  INTERFACE
     FUNCTION SLJEV2(A11, A22, A21, CS, SN, L1, L2, ES) BIND(C,NAME='pvn_sljev2_')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float, c_int
       IMPLICIT NONE
       REAL(c_float), INTENT(IN), TARGET :: A11, A22, A21
       REAL(c_float), INTENT(OUT), TARGET :: CS, SN, L1, L2
       INTEGER(c_int), INTENT(INOUT), TARGET :: ES
       INTEGER(c_int) :: SLJEV2
     END FUNCTION SLJEV2
  END INTERFACE

  REAL(c_float), PARAMETER :: ZERO = 0.0_c_float, ONE = 1.0_c_float
  REAL(c_float), INTENT(IN), TARGET :: A, B, C
  REAL(c_float), INTENT(OUT), TARGET :: RT1, RT2, CS1, SN1
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER(c_int), TARGET :: ES

  ES = INT(INFO, c_int)
  INFO = INT(SLJEV2(A, C, B, CS1, SN1, RT1, RT2, ES))
  IF (INFO .LT. 0) THEN
     CS1 = ONE
     SN1 = ZERO
     RT1 = ZERO
     RT2 = ZERO
  ELSE IF (ES .NE. 0_c_int) THEN
     RT1 = SCALE(RT1, ES)
     RT2 = SCALE(RT2, ES)
  END IF
END SUBROUTINE SJIEV2

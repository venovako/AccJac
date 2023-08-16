SUBROUTINE WJAEV2(A, B, C, RT1, RT2, CS1, SN1)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double, c_int
  IMPLICIT NONE

  INTERFACE
     FUNCTION WLJEV2(A11, A22, A21R, A21I, CS, SNR, SNI, L1, L2, ES) BIND(C,NAME='pvn_wljev2_')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double, c_int
       IMPLICIT NONE
       REAL(c_long_double), INTENT(IN), TARGET :: A11, A22, A21R, A21I
       REAL(c_long_double), INTENT(OUT), TARGET :: CS, SNR, SNI, L1, L2
       INTEGER(c_int), INTENT(INOUT), TARGET :: ES
       INTEGER(c_int) :: WLJEV2
     END FUNCTION WLJEV2
  END INTERFACE

  REAL(c_long_double), PARAMETER :: ZERO = 0.0_c_long_double, ONE = 1.0_c_long_double
  COMPLEX(c_long_double), INTENT(IN) :: A, B, C
  REAL(c_long_double), INTENT(OUT), TARGET :: RT1, RT2, CS1
  COMPLEX(c_long_double), INTENT(OUT) :: SN1
  REAL(c_long_double), TARGET :: A11, A22, A21R, A21I, SNR, SNI
  INTEGER(c_int), TARGET :: ES

  A11 = REAL(A)
  SNR = AIMAG(A)
  A22 = REAL(C)
  SNI = AIMAG(C)
  A21R = REAL(B)
  A21I = -AIMAG(B)
  ES = 0_c_int

  IF ((SNR .NE. ZERO) .OR. (SNI .NE. ZERO) .OR. (WLJEV2(A11, A22, A21R, A21I, CS1, SNR, SNI, RT1, RT2, ES) .LT. 0_c_int)) THEN
     CS1 = ONE
     SN1 = CMPLX(ZERO, ZERO, c_long_double)
     RT1 = ZERO
     RT2 = ZERO
  ELSE IF (ES .NE. 0_c_int) THEN
     RT1 = SCALE(RT1, ES)
     RT2 = SCALE(RT2, ES)
  END IF
END SUBROUTINE WJAEV2

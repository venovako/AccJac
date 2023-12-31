SUBROUTINE ZJIEV2(A, B, C, RT1, RT2, CS1, SN1, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double, c_int
  IMPLICIT NONE

  INTERFACE
     FUNCTION ZLJEV2(A11, A22, A21R, A21I, CS, SNR, SNI, L1, L2, ES) BIND(C,NAME='pvn_zljev2_')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double, c_int
       IMPLICIT NONE
       REAL(c_double), INTENT(IN), TARGET :: A11, A22, A21R, A21I
       REAL(c_double), INTENT(OUT), TARGET :: CS, SNR, SNI, L1, L2
       INTEGER(c_int), INTENT(INOUT), TARGET :: ES
       INTEGER(c_int) :: ZLJEV2
     END FUNCTION ZLJEV2
  END INTERFACE

  REAL(c_double), PARAMETER :: ZERO = 0.0_c_double, ONE = 1.0_c_double
  COMPLEX(c_double), INTENT(IN) :: A, B, C
  REAL(c_double), INTENT(OUT), TARGET :: RT1, RT2, CS1
  COMPLEX(c_double), INTENT(OUT) :: SN1
  REAL(c_double), TARGET :: A11, A22, A21R, A21I, SNR, SNI
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER(c_int), TARGET :: ES

  A11 = REAL(A)
  SNR = AIMAG(A)
  A22 = REAL(C)
  SNI = AIMAG(C)
  A21R = REAL(B)
  A21I = -AIMAG(B)

  ES = INT(INFO, c_int)
  IF (SNR .NE. ZERO) THEN
     INFO = -1
  ELSE IF (SNI .NE. ZERO) THEN
     INFO = -3
  ELSE ! all OK
     INFO = INT(ZLJEV2(A11, A22, A21R, A21I, CS1, SNR, SNI, RT1, RT2, ES))
  END IF

  IF (INFO .LT. 0) THEN
     CS1 = ONE
     SN1 = CMPLX(ZERO, ZERO, c_double)
     RT1 = ZERO
     RT2 = ZERO
  ELSE ! all OK
     SN1 = CMPLX(SNR, SNI, c_double)
     IF (ES .NE. 0_c_int) THEN
        RT1 = SCALE(RT1, ES)
        RT2 = SCALE(RT2, ES)
     END IF
  END IF
END SUBROUTINE ZJIEV2

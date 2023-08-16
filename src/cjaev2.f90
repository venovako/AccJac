SUBROUTINE CJAEV2(A, B, C, RT1, RT2, CS1, SN1)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float, c_int
  IMPLICIT NONE

  INTERFACE
     FUNCTION CLJEV2(A11, A22, A21R, A21I, CS, SNR, SNI, L1, L2, ES) BIND(C,NAME='pvn_cljev2_')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float, c_int
       IMPLICIT NONE
       REAL(c_float), INTENT(IN), TARGET :: A11, A22, A21R, A21I
       REAL(c_float), INTENT(OUT), TARGET :: CS, SNR, SNI, L1, L2
       INTEGER(c_int), INTENT(INOUT), TARGET :: ES
       INTEGER(c_int) :: CLJEV2
     END FUNCTION CLJEV2
  END INTERFACE

  REAL(c_float), PARAMETER :: ZERO = 0.0_c_float, ONE = 1.0_c_float
  COMPLEX(c_float), INTENT(IN) :: A, B, C
  REAL(c_float), INTENT(OUT), TARGET :: RT1, RT2, CS1
  COMPLEX(c_float), INTENT(OUT) :: SN1
  REAL(c_float), TARGET :: A11, A22, A21R, A21I, SNR, SNI
  INTEGER(c_int), TARGET :: ES

  A11 = REAL(A)
  SNR = AIMAG(A)
  A22 = REAL(C)
  SNI = AIMAG(C)
  A21R = REAL(B)
  A21I = -AIMAG(B)
  ES = 0_c_int

  IF ((SNR .NE. ZERO) .OR. (SNI .NE. ZERO) .OR. (CLJEV2(A11, A22, A21R, A21I, CS1, SNR, SNI, RT1, RT2, ES) .LT. 0_c_int)) THEN
     CS1 = ONE
     SN1 = CMPLX(ZERO, ZERO, c_float)
     RT1 = ZERO
     RT2 = ZERO
  ELSE ! all OK
     SN1 = CMPLX(SNR, SNI, c_float)
     IF (ES .NE. 0_c_int) THEN
        RT1 = SCALE(RT1, ES)
        RT2 = SCALE(RT2, ES)
     END IF
  END IF
END SUBROUTINE CJAEV2

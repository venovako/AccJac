SUBROUTINE WJAEV2(A, B, C, RT1, RT2, CS1, SN1)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int, c_long_double
  IMPLICIT NONE

  REAL(c_long_double), PARAMETER :: ZERO = 0.0_c_long_double, ONE = 1.0_c_long_double
  COMPLEX(c_long_double), INTENT(IN) :: A, B, C
  REAL(c_long_double), INTENT(OUT) :: RT1, RT2, CS1
  COMPLEX(c_long_double), INTENT(OUT) :: SN1
  REAL(c_long_double) :: A11, A22, A21R, A21I, SNR, SNI
  INTEGER(c_int) :: ES
  INTEGER(c_int), EXTERNAL :: PVN_WLJEV2

  A11 = REAL(A)
  SNR = AIMAG(A)
  A22 = REAL(C)
  SNI = AIMAG(C)
  A21R = REAL(B)
  A21I = -AIMAG(B)
  ES = 0_c_int

  IF ((SNR .NE. ZERO) .OR. (SNI .NE. ZERO) .OR. (PVN_WLJEV2(A11, A22, A21R,A21I, CS1, SNR,SNI, RT1, RT2, ES) .LT. 0_c_int)) THEN
     CS1 = ONE
     SN1 = CMPLX(ZERO, ZERO, c_long_double)
     RT1 = ZERO
     RT2 = ZERO
  ELSE ! all OK
     SN1 = CMPLX(SNR, SNI, c_long_double)
     IF (ES .NE. 0_c_int) THEN
        RT1 = SCALE(RT1, ES)
        RT2 = SCALE(RT2, ES)
     END IF
  END IF
END SUBROUTINE WJAEV2

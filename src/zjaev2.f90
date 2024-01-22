SUBROUTINE ZJAEV2(A, B, C, RT1, RT2, CS1, SN1)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double, c_int
  IMPLICIT NONE

  REAL(c_double), PARAMETER :: ZERO = 0.0_c_double, ONE = 1.0_c_double
  COMPLEX(c_double), INTENT(IN) :: A, B, C
  REAL(c_double), INTENT(OUT) :: RT1, RT2, CS1
  COMPLEX(c_double), INTENT(OUT) :: SN1
  REAL(c_double) :: A11, A22, A21R, A21I, SNR, SNI
  INTEGER(c_int) :: ES
  INTEGER(c_int), EXTERNAL :: PVN_ZLJEV2

  A11 = REAL(A)
  SNR = AIMAG(A)
  A22 = REAL(C)
  SNI = AIMAG(C)
  A21R = REAL(B)
  A21I = -AIMAG(B)
  ES = 0_c_int

  IF ((SNR .NE. ZERO) .OR. (SNI .NE. ZERO) .OR. (PVN_ZLJEV2(A11, A22, A21R,A21I, CS1, SNR,SNI, RT1, RT2, ES) .LT. 0_c_int)) THEN
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
END SUBROUTINE ZJAEV2

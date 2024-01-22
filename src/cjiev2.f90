SUBROUTINE CJIEV2(A, B, C, RT1, RT2, CS1, SN1, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float, c_int
  IMPLICIT NONE

  REAL(c_float), PARAMETER :: ZERO = 0.0_c_float, ONE = 1.0_c_float
  COMPLEX(c_float), INTENT(IN) :: A, B, C
  REAL(c_float), INTENT(OUT) :: RT1, RT2, CS1
  COMPLEX(c_float), INTENT(OUT) :: SN1
  INTEGER, INTENT(INOUT) :: INFO
  REAL(c_float) :: A11, A22, A21R, A21I, SNR, SNI
  INTEGER(c_int) :: ES
  INTEGER(c_int), EXTERNAL :: PVN_CLJEV2

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
     INFO = INT(PVN_CLJEV2(A11, A22, A21R, A21I, CS1, SNR, SNI, RT1, RT2, ES))
  END IF

  IF (INFO .LT. 0) THEN
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
END SUBROUTINE CJIEV2

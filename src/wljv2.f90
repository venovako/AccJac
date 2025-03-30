SUBROUTINE WLJV2(A11, A22, A21R, A21I, CH, SHR, SHI, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  REAL(KIND=10), PARAMETER :: ZERO = 0.0_10, ONE = 1.0_10
  REAL(KIND=10), PARAMETER :: CUTTH = 4.0_10 / 5.0_10
  REAL(KIND=10), PARAMETER :: CUTCH = 5.0_10 / 3.0_10
  REAL(KIND=10), PARAMETER :: CUTSH = 4.0_10 / 3.0_10
  REAL(KIND=10), INTENT(IN) :: A11, A22, A21R, A21I
  REAL(KIND=10), INTENT(OUT) :: CH, SHR, SHI
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=10) :: A
  INTEGER(KIND=c_int) :: ES
  INTEGER(KIND=c_int), EXTERNAL :: PVN_WLJV2
  ES = INT(INFO, c_int)
  INFO = INT(PVN_WLJV2(A11, A22, A21R, A21I, CH, SHR, SHI, ES))
  IF (INFO .GE. 0) THEN
     INFO = IAND(INFO, 1)
     A = CR_HYPOT(SHR, SHI)
     IF (.NOT. (A .LE. HUGE(A))) THEN
        ! |TH| >= 1 => skip the transformation
        CH = ONE
        SHR = ZERO
        SHI = ZERO
        INFO = IOR(INFO, 4)
     ELSE IF (INFO .EQ. 0) THEN
        IF (A .GE. (CUTTH * CH)) THEN
           CH = CUTCH
           SHR = (SHR / A) * CUTSH
           SHI = (SHI / A) * CUTSH
           INFO = 2
        END IF
     ELSE ! SH => TH
        IF (A .GE. CUTTH) THEN
           CH = CUTCH
           SHR = (SHR / A) * CUTTH
           SHI = (SHI / A) * CUTTH
           INFO = 3
        END IF
     END IF
  END IF
END SUBROUTINE WLJV2

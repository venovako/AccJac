SUBROUTINE ZLJV2(A11, A22, A21R, A21I, CH, SHR, SHI, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       IMPLICIT NONE
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64, ONE = 1.0_REAL64
  REAL(KIND=REAL64), PARAMETER :: CUTTH = 4.0_REAL64 / 5.0_REAL64
  REAL(KIND=REAL64), PARAMETER :: CUTCH = 5.0_REAL64 / 3.0_REAL64
  REAL(KIND=REAL64), PARAMETER :: CUTSH = 4.0_REAL64 / 3.0_REAL64
  REAL(KIND=REAL64), INTENT(IN) :: A11, A22, A21R, A21I
  REAL(KIND=REAL64), INTENT(OUT) :: CH, SHR, SHI
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=REAL64) :: A
  INTEGER(KIND=c_int) :: ES
  INTEGER(KIND=c_int), EXTERNAL :: PVN_ZLJV2
  ES = INT(INFO, c_int)
  INFO = INT(PVN_ZLJV2(A11, A22, A21R, A21I, CH, SHR, SHI, ES))
  IF (INFO .GE. 0) THEN
     INFO = IAND(INFO, 1)
     A = CR_HYPOT(SHR, SHI)
     IF (.NOT. (A .LE. HUGE(A))) THEN
        ! |TH| >= 1 => skip the transformation
        CH = ONE
        SHR = ZERO
        SHI = ZERO
        INFO = INFO + 4
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
END SUBROUTINE ZLJV2

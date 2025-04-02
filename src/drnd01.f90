! returns pseudorandom double precision X, 0 <= X < 1, X normal
FUNCTION DRND01()
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL64
  IMPLICIT NONE
  INTERFACE
     ! 0 <= result <= 2^31 - 1
     FUNCTION RANDOM() BIND(C,NAME='random')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long
       IMPLICIT NONE
       INTEGER(KIND=c_long) :: RANDOM
     END FUNCTION RANDOM
  END INTERFACE
  REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64, HALF = 0.5_REAL64, ONE = 1.0_REAL64
  INTEGER(KIND=INT64) :: I
  REAL(KIND=REAL64) :: D, DRND01
  EQUIVALENCE(D, I)
  ! this makes the bits 63 and 62 zero, and the rest are random, i.e.,
  ! 0 <= D < 2
  I = IOR(ISHFT(RANDOM(), 31), RANDOM())
  ! this might saturate results somewhat, just above tiny and just below one
  IF ((D .GT. ZERO) .AND. (D .LT. TINY(D))) THEN
     D = SCALE(FRACTION(D), EXPONENT(TINY(D)))
  ELSE IF (D .GE. ONE) THEN
     D = D * HALF
  END IF
  DRND01 = D
END FUNCTION DRND01

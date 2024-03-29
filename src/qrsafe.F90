FUNCTION QRSAFE(U)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: U
  REAL(REAL128) :: QRSAFE, RMIN, RMAX, R, A
  RMIN = TINY(A)
  A = HUGE(A)
  RMAX = SCALE(A, -2)
  DO WHILE ((.NOT. (A .GE. RMIN)) .OR. (.NOT. (A .LE. RMAX)))
     READ (U) R
     A = ABS(R)
  END DO
  QRSAFE = R
END FUNCTION QRSAFE

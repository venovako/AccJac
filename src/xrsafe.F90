FUNCTION XRSAFE(U)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: U
  REAL(c_long_double) :: XRSAFE
  REAL(c_long_double) :: RMIN, RMAX, R, A
  RMIN = TINY(A)
  A = HUGE(A)
  RMAX = SCALE(A, -2)
  DO WHILE ((.NOT. (A .GE. RMIN)) .OR. (.NOT. (A .LE. RMAX)))
     READ (U) R
     A = ABS(R)
  END DO
  XRSAFE = R
END FUNCTION XRSAFE

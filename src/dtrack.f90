SUBROUTINE DTRACK(N, SV, GX, GS, SWP, NTR)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, REAL64
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL64
  INTEGER, INTENT(IN) :: N, GS, SWP, NTR
  REAL(KIND=K), INTENT(IN) :: SV(N), GX
  INTEGER :: J
  WRITE (ERROR_UNIT,'(I10,I11,I7,ES25.17E3)') SWP, NTR, GS, GX
  DO J = 1, N
     WRITE (ERROR_UNIT,'(I3,ES25.17E3)') J, SV(J)
  END DO
END SUBROUTINE DTRACK

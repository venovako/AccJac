SUBROUTINE XTRACK(N, SV, GX, GS, SWP, NTR, U)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#endif
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INPUT_UNIT, REAL128
  IMPLICIT NONE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  INTEGER, INTENT(IN) :: N, GS, SWP, NTR, U
  REAL(KIND=K), INTENT(IN) :: SV(N), GX
  INTEGER :: I, J
  IF (U .EQ. INPUT_UNIT) RETURN
#ifdef __GFORTRAN__
  WRITE (U,'(I10,A,I11,A,I6,A,ES30.21E4)') SWP, ',', ABS(NTR), ',', GS, ',', GX
#else
  WRITE (U,'(I10,A,I11,A,I6,A,ES45.36E4)') SWP, ',', ABS(NTR), ',', GS, ',', GX
#endif
  FLUSH(U)
  IF ((NTR .GT. 0) .AND. (N .LT. 1000)) THEN
     I = -GS
     DO J = 1, N
#ifdef __GFORTRAN__
        WRITE (U,'(I3,A,ES30.21E4)') J, ',', SCALE(REAL(SV(J), REAL128), I)
#else
        WRITE (U,'(I3,A,ES45.36E4)') J, ',', SCALE(SV(J), I)
#endif
        FLUSH(U)
     END DO
  END IF
END SUBROUTINE XTRACK

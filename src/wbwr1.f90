!>@brief \b WBWR1 writes a one-dimensional quadruple/extended precision complex array to a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the length of G.
!!@param G [IN]; a quadruple/extended precision complex array to be written.
!!@param INFO [OUT]; zero on success, -i if the i-th argument had an illegal value, or a positive I/O error code.
SUBROUTINE WBWR1(U, M, G, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double, c_short
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double, L = K / c_short
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  INTEGER, INTENT(IN) :: U, M
  COMPLEX(KIND=K), INTENT(IN) :: G(M)
  INTEGER, INTENT(OUT) :: INFO
#ifdef __GFORTRAN__
  INTEGER :: I
  REAL(KIND=K) :: X
  INTEGER(KIND=c_short) :: E(L)
  EQUIVALENCE(X,E)
#endif
  INFO = 0
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
#ifdef __GFORTRAN__
  DO I = 1, M
     X = REAL(G(I))
     WRITE (UNIT=U, IOSTAT=INFO) E
     IF (INFO .NE. 0) EXIT
     X = AIMAG(G(I))
     WRITE (UNIT=U, IOSTAT=INFO) E
     IF (INFO .NE. 0) EXIT
  END DO
#else
  WRITE (UNIT=U, IOSTAT=INFO) G
#endif
END SUBROUTINE WBWR1

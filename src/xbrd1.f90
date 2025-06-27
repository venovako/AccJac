!>@brief \b XBRD1 reads a one-dimensional quadruple/extended precision real array from a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the length of G.
!!@param G [OUT]; a quadruple/extended precision real array to be read.
!!@param INFO [OUT]; zero on success, -i if the i-th argument had an illegal value, or a positive I/O error code.
SUBROUTINE XBRD1(U, M, G, INFO)
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
  REAL(KIND=K), INTENT(OUT) :: G(M)
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
     READ (UNIT=U, IOSTAT=INFO) E
     IF (INFO .NE. 0) EXIT
     G(I) = X
  END DO
#else
  READ (UNIT=U, IOSTAT=INFO) G
#endif
END SUBROUTINE XBRD1

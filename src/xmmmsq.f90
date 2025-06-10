! C = A * B
PURE SUBROUTINE XMMMSQ(N, A, LDA, B, LDB, C, LDC)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION XFMA(A, B, C)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: A, B, C
       REAL(KIND=c_long_double) :: XFMA
#else
       REAL(KIND=REAL128), INTENT(IN) :: A, B, C
       REAL(KIND=REAL128) :: XFMA
#endif
     END FUNCTION XFMA
  END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: N, LDA, LDB, LDC
  REAL(KIND=K), INTENT(IN) :: A(LDA,N), B(LDB,N)
  REAL(KIND=K), INTENT(OUT) :: C(LDC,N)
  INTEGER :: I, J, L
  DO J = 1, N
     DO I = 1, N
        C(I,J) = ZERO
     END DO
  END DO
  DO J = 1, N
     DO L = 1, N
        DO I = 1, N
           C(I,J) = XFMA(A(I,L), B(L,J), C(I,J))
        END DO
     END DO
  END DO
END SUBROUTINE XMMMSQ

! C = A * B
PURE SUBROUTINE DMMMSQ(N, A, LDA, B, LDB, C, LDC)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION DFMA(A, B, C)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       REAL(KIND=REAL64), INTENT(IN) :: A, B, C
       REAL(KIND=REAL64) :: DFMA
     END FUNCTION DFMA
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: N, LDA, LDB, LDC
  REAL(KIND=K), INTENT(IN) :: A(LDA,N), B(LDB,N)
  REAL(KIND=K), INTENT(OUT) :: C(LDC,N)
  INTEGER :: I, J, L
  DO J = 1, N
     DO I = 1, N
        C(I,J) = ZERO
        DO L = 1, N
           C(I,J) = DFMA(A(I,L), B(L,J), C(I,J))
        END DO
     END DO
  END DO
END SUBROUTINE DMMMSQ

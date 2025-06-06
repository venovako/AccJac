! C = A * B
PURE SUBROUTINE YMMMSQ(N, A, LDA, B, LDB, C, LDC)
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL128
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: N, LDA, LDB, LDC
  COMPLEX(KIND=K), INTENT(IN) :: A(LDA,N), B(LDB,N)
  COMPLEX(KIND=K), INTENT(OUT) :: C(LDC,N)
  INTEGER :: I, J, L
  DO J = 1, N
     DO I = 1, N
        C(I,J) = ZERO
        DO L = 1, N
           C(I,J) = CMPLX(&
                IEEE_FMA(REAL(A(I,L)), REAL(B(L,J)), IEEE_FMA(-AIMAG(A(I,L)), AIMAG(B(L,J)), REAL(C(I,J)))),&
                IEEE_FMA(REAL(A(I,L)), AIMAG(B(L,J)), IEEE_FMA(AIMAG(A(I,L)), REAL(B(L,J)), AIMAG(C(I,J)))), K)
        END DO
     END DO
  END DO
END SUBROUTINE YMMMSQ

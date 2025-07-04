! C += A * B
#ifdef _OPENMP
SUBROUTINE JMMMSQ(M, N, A, LDA, B, LDB, C, LDC)
#else
PURE SUBROUTINE JMMMSQ(M, N, A, LDA, B, LDB, C, LDC)
#endif
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = INT64
  INTEGER, INTENT(IN) :: M, N, LDA, LDB, LDC
  INTEGER(KIND=K), INTENT(IN) :: A(LDA,M), B(LDB,N)
  INTEGER(KIND=K), INTENT(INOUT) :: C(LDC,N)
  INTEGER :: I, J, L
  !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,L) SHARED(A,B,C,M,N)
  DO J = 1, N
     DO L = 1, M
        DO I = 1, N
           C(I,J) = A(I,L) * B(L,J) + C(I,J)
        END DO
     END DO
  END DO
  !$OMP END PARALLEL DO
END SUBROUTINE JMMMSQ

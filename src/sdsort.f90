PURE SUBROUTINE SDSORT(N, A, LDA, V, LDV, JPOS, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE SSWPC(N, A, LDA, P, Q, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       REAL(KIND=REAL32), INTENT(INOUT) :: A(LDA,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE SSWPC
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE SSWPR(N, A, LDA, P, Q, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       REAL(KIND=REAL32), INTENT(INOUT) :: A(LDA,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE SSWPR
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL32
  INTEGER, INTENT(IN) :: N, LDA, LDV, JPOS
  REAL(KIND=K), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=K) :: X
  INTEGER :: I, J, L, M
#define SWPC SSWPC
#define SWPR SSWPR
#include "gdsort.f90"
END SUBROUTINE SDSORT

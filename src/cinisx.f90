#ifdef _OPENMP
SUBROUTINE CINISX(M, N, G, LDG, V, LDV, SV, IX, INFO)
#else
PURE SUBROUTINE CINISX(M, N, G, LDG, V, LDV, SV, IX, INFO)
#endif
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION CNRMF(M, X)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       COMPLEX(KIND=REAL32), INTENT(IN) :: X(M)
       REAL(KIND=REAL32) :: CNRMF
     END FUNCTION CNRMF
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL32
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, LDV
  COMPLEX(KIND=K), INTENT(IN) :: G(LDG,N)
  COMPLEX(KIND=K), INTENT(OUT) :: V(LDV,N)
  REAL(KIND=K), INTENT(OUT) :: SV(N)
  INTEGER, INTENT(OUT) :: IX(N)
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER :: I, J
#define NRMF CNRMF
#include "ginisx.f90"
END SUBROUTINE CINISX

#ifdef _OPENMP
SUBROUTINE ZCNRMF(M, N, G, LDG, SV, IX, INFO)
#else
PURE SUBROUTINE ZCNRMF(M, N, G, LDG, SV, IX, INFO)
#endif
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION ZNRMF(M, X)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       COMPLEX(KIND=REAL64), INTENT(IN) :: X(M)
       REAL(KIND=REAL64) :: ZNRMF
     END FUNCTION ZNRMF
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  INTEGER, INTENT(IN) :: M, N, LDG, IX(N)
  COMPLEX(KIND=K), INTENT(IN) :: G(LDG,N)
  REAL(KIND=K), INTENT(OUT) :: SV(N)
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: J, L
#define NRMF ZNRMF
#include "gcnrmf.f90"
END SUBROUTINE ZCNRMF

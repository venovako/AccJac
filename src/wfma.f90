PURE FUNCTION WFMA(A, B, C)
  ! USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  ! USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  COMPLEX(KIND=c_long_double), INTENT(IN) :: A, B, C
  COMPLEX(KIND=c_long_double) :: WFMA
  ! REAL(KIND=REAL128) :: AR, AI, BR, BI, CR, CI
  ! REAL(KIND=c_long_double) :: DR, DI
  ! AR = REAL(A)
  ! AI = AIMAG(A)
  ! BR = REAL(B)
  ! BI = AIMAG(B)
  ! CR = REAL(C)
  ! CI = AIMAG(C)
  ! DR = REAL(IEEE_FMA(AR, BR, IEEE_FMA(-AI, BI, CR)), c_long_double)
  ! DI = REAL(IEEE_FMA(AR, BI, IEEE_FMA( AI, BR, CI)), c_long_double)
  ! WFMA = CMPLX(DR, DI, c_long_double)
  WFMA = A * B + C
END FUNCTION WFMA

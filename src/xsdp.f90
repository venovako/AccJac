FUNCTION XSDP(M, X, Y, MX, MY, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = c_long_double
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: M
  REAL(KIND=K), INTENT(IN) :: X(M), Y(M), MX, MY
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=K) :: XSDP
  INTEGER :: I
  INFO = 0
  XSDP = ZERO
  IF (.NOT. (MY .GT. ZERO)) INFO = -5
  IF (.NOT. (MX .GT. ZERO)) INFO = -4
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  DO I = 1, M
     XSDP = XSDP + (X(I) / MX) * (Y(I) / MY)
  END DO
END FUNCTION XSDP

SUBROUTINE SROTT(M, X, Y, CS, SN, MX, MY, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  REAL(KIND=REAL32), PARAMETER :: ZERO = 0.0_REAL32
  INTEGER, INTENT(IN) :: M
  REAL(KIND=REAL32), INTENT(INOUT) :: X(M), Y(M)
  REAL(KIND=REAL32), INTENT(IN) :: CS, SN
  REAL(KIND=REAL32), INTENT(OUT) :: MX, MY
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=REAL32) :: XX, YY
  INTEGER :: I
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  MX = ZERO
  MY = ZERO
  IF (INFO .EQ. 0) THEN
     !DIR$ VECTOR ALWAYS
     DO I = 1, M
        XX = X(I) * CS + Y(I) * SN
        YY = Y(I) * CS - X(I) * SN
        X(I) = XX
        Y(I) = YY
        MX = MAX(MX, ABS(XX))
        MY = MAX(MY, ABS(YY))
     END DO
  ELSE ! INFO > 0
     INFO = 0
     ! SN => TH
     !DIR$ VECTOR ALWAYS
     DO I = 1, M
        !DIR$ FMA
        XX = (X(I) + Y(I) * SN) * CS
        !DIR$ FMA
        YY = (Y(I) - X(I) * SN) * CS
        X(I) = XX
        Y(I) = YY
        MX = MAX(MX, ABS(XX))
        MY = MAX(MY, ABS(YY))
     END DO
  END IF
END SUBROUTINE SROTT

PURE SUBROUTINE XPRCYC(M, N, G, LDG, JPOS, SV, IX, WRK, RWRK, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE XCNRMF(M, N, G, LDG, SV, IX, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, IX(N)
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: G(LDG,N)
       REAL(KIND=c_long_double), INTENT(OUT) :: SV(N)
#else
       REAL(KIND=REAL128), INTENT(IN) :: G(LDG,N)
       REAL(KIND=REAL128), INTENT(OUT) :: SV(N)
#endif
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE XCNRMF
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XNSORT(N, JPOS, SV, IX, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, JPOS
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: SV(N)
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: SV(N)
#endif
       INTEGER, INTENT(INOUT) :: IX(N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE XNSORT
  END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, JPOS
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(OUT) :: SV(N), WRK(M,N), RWRK(N)
  INTEGER, INTENT(INOUT) :: IX(N), INFO
  INTEGER :: I, J
#ifndef NDEBUG
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -5
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  IF (INFO .NE. 0) THEN
     IF (INFO .EQ. 2) THEN
        DO J = 1, N
           DO I = 1, M
              WRK(I,J) = ZERO
           END DO
        END DO
        DO I = 1, N-1
           RWRK(I) = ONE
        END DO
#ifndef NDEBUG
        RWRK(N) = REAL(INFO, K)
     ELSE IF (INFO .NE. 1) THEN
        INFO = -9
        RETURN
#endif
     END IF
     CALL XCNRMF(M, N, G, LDG, SV, IX, INFO)
#ifndef NDEBUG
     IF (INFO .NE. 0) THEN
        INFO = -6
        RETURN
     END IF
#endif
  END IF
  CALL XNSORT(N, JPOS, SV, IX, INFO)
#ifndef NDEBUG
  IF (INFO .LT. 0) THEN
     INFO = -7
     RETURN
  END IF
#endif
END SUBROUTINE XPRCYC

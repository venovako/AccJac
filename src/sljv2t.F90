PROGRAM SLJV2T
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int, c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, REAL32
#else
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, REAL32, REAL128
#endif
  IMPLICIT NONE
#ifdef __GFORTRAN__
  INTERFACE
     PURE FUNCTION HYPOTX(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: HYPOTX
     END FUNCTION HYPOTX
  END INTERFACE
  INTEGER, PARAMETER :: KK = c_long_double
#else
#define HYPOTX HYPOT
  INTEGER, PARAMETER :: KK = REAL128
#endif
  REAL(KIND=KK), PARAMETER :: QZERO = 0.0_KK, QONE = 1.0_KK
  REAL(KIND=REAL32), PARAMETER :: ZERO = 0.0_REAL32, CUTOFF = 0.8_REAL32, SEPS = EPSILON(ZERO) / 2
  ! DAMP should counterweigh a possible unfavorable rounding when creating the off-diagonal element.
  ! This has been observed in single precision, and is more unlikely in higher precisions.
  REAL(KIND=REAL32), PARAMETER :: DAMP = 1.0_REAL32 - 4 * EPSILON(ZERO)
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=KK) :: Q(10), W
  REAL(KIND=REAL32) :: D(5), T
  INTEGER, ALLOCATABLE :: ISEED(:)
  !DIR$ ATTRIBUTES ALIGN: 64:: ISEED
  INTEGER :: I, N, SSIZE
  INTEGER(KIND=c_int) :: ES
  INTEGER(KIND=c_int), EXTERNAL :: PVN_SLJV2
#ifdef __GFORTRAN__
  INTEGER(KIND=c_int), EXTERNAL :: PVN_XLJV2
#else
  INTEGER(KIND=c_int), EXTERNAL :: PVN_QLJV2
#endif
  ! random seed may be given
  CALL RANDOM_SEED(SIZE=SSIZE)
  IF (SSIZE .LE. 0) STOP 'seed size non-positive'
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .LT. 1) THEN
     IF (SSIZE .GT. 1) THEN
        WRITE (CLA,'(I1)') SSIZE
        CLA = 'args: N [SEED1 ... SEED'//TRIM(CLA)//']'
     ELSE ! SSIZE = 1
        CLA = 'args: N [SEED1]'
     END IF
     WRITE (ERROR_UNIT,*) TRIM(CLA)
     STOP 'All SEED arguments have to be given, or none of them.'
  END IF
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) N
  IF (I .EQ. 1) THEN
     ALLOCATE(ISEED(SSIZE))
     CALL RANDOM_SEED
     CALL RANDOM_SEED(GET=ISEED)
  ELSE IF (I .EQ. (SSIZE + 1)) THEN
     ALLOCATE(ISEED(SSIZE))
     DO I = 1, SSIZE
        CALL GET_COMMAND_ARGUMENT(I + 1, CLA)
        READ (CLA,*) ISEED(I)
     END DO
     CALL RANDOM_SEED(PUT=ISEED)
  ELSE ! a wrong SEED
     STOP 'invalid number of SEED arguments'
  END IF
  DO I = 1, SSIZE-1
     WRITE (ERROR_UNIT,'(I12)',ADVANCE='NO') ISEED(I)
  END DO
  WRITE (ERROR_UNIT,'(I12)') ISEED(SSIZE)
  Q = QZERO
  D = ZERO
  ISEED = 0
  DO I = 1, ABS(N)
1    CALL RANDOM_NUMBER(D)
     D(1) = D(1) / D(3)
     IF (.NOT. (D(1) .LE. HUGE(ZERO))) GOTO 1
     D(2) = D(2) / D(4)
     IF (.NOT. (D(2) .LE. HUGE(ZERO))) GOTO 1
     SSIZE = MOD(EXPONENT(D(3)), 2)
     D(3) = SQRT(D(1)) * SQRT(D(2)) * MIN(D(5), DAMP)
     IF (.NOT. (D(3) .LE. HUGE(ZERO))) GOTO 1
     IF (SSIZE .NE. 0) D(3) = -D(3)
     ES = 0_c_int
     SSIZE = INT(PVN_SLJV2(D(1), D(2), D(3), D(4), D(5), T, ES))
     IF (SSIZE .LT. 0) THEN
        WRITE (ERROR_UNIT,'(I11,A,I3)') I, ': error', SSIZE
        GOTO 2
     END IF
     IF (ABS(D(5)) .GE. (CUTOFF * D(4))) THEN
        ISEED(1) = ISEED(1) + 1
        IF (N .LT. 0) GOTO 1
     END IF
     Q(4) = D(4) ! CH
     Q(5) = D(5) ! SH
     Q(6) = HYPOTX(Q(5), QONE)
     Q(6) = ABS((Q(4) - Q(6)) * (Q(4) + Q(6)))
     Q(1) = MAX(Q(1), Q(6))
     Q(8) = D(1)
     Q(9) = D(2)
     Q(10) = D(3)
     ES = 0_c_int
#ifdef __GFORTRAN__
     SSIZE = INT(PVN_XLJV2(Q(8), Q(9), Q(10), Q(6), Q(7), W, ES))
#else
     SSIZE = INT(PVN_QLJV2(Q(8), Q(9), Q(10), Q(6), Q(7), W, ES))
#endif
     IF (SSIZE .LT. 0) THEN
        WRITE (ERROR_UNIT,'(I11,A,I3)') I, ': ERROR', SSIZE
        GOTO 2
     END IF
     Q(4) = ABS(Q(4) - Q(6)) / Q(6)
     Q(2) = MAX(Q(2), Q(4))
     Q(5) = ABS(Q(5) - Q(7))
     IF ((Q(7) .NE. QZERO) .OR. (Q(5) .NE. QZERO)) Q(5) = Q(5) / Q(7)
     Q(3) = MAX(Q(3), Q(5))
  END DO
  ! relative errors in the terms of \epsilon
  DO I = 1, 3
     Q(I) = Q(I) / SEPS
  END DO
  IF (N .LT. 0) THEN
     WRITE (OUTPUT_UNIT,'(I11,A,I11)',ADVANCE='NO') -ISEED(1), ',', -N
  ELSE ! N >= 0
     WRITE (OUTPUT_UNIT,'(I11,A,I11)',ADVANCE='NO')  ISEED(1), ',',  N
  END IF
#ifdef __GFORTRAN__
  WRITE (OUTPUT_UNIT,'(3(A,ES16.9E2))') ',', Q(1), ',', Q(2), ',', Q(3)
#else
  WRITE (OUTPUT_UNIT,'(3(A,ES16.9E2))') ',',&
       REAL(Q(1), REAL32), ',', REAL(Q(2), REAL32), ',', REAL(Q(3), REAL32)
#endif
2 DEALLOCATE(ISEED)
END PROGRAM SLJV2T

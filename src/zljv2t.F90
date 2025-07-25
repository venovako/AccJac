PROGRAM ZLJV2T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, REAL64, REAL128
  IMPLICIT NONE
#include "cr.f90"
  INTEGER, PARAMETER :: K = REAL64, KK = REAL128
  REAL(KIND=KK), PARAMETER :: QZERO = 0.0_KK, QONE = 1.0_KK
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, DEPS = EPSILON(ZERO) / 2
  ! DAMP should counterweigh a possible unfavorable rounding when creating the off-diagonal element.
  ! This has been observed in single precision, and is more unlikely in higher precisions.
  REAL(KIND=K), PARAMETER :: DAMP = 1.0_K - 8 * EPSILON(ZERO)
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=KK) :: Q(14), W
  REAL(KIND=K) :: D(7), T
  INTEGER, ALLOCATABLE :: ISEED(:)
  INTEGER :: I, N, SSIZE
  INTEGER(KIND=c_int) :: ES
  INTEGER(KIND=c_int), EXTERNAL :: PVN_ZLJV2, PVN_YLJV2
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
     T = SQRT(D(1)) * SQRT(D(2))
     SSIZE = MOD(EXPONENT(D(3)), 2)
     D(3) = T * MIN(D(5), DAMP)
     IF (.NOT. (D(3) .LE. HUGE(ZERO))) GOTO 1
     IF (SSIZE .NE. 0) D(3) = -D(3)
     SSIZE = MOD(EXPONENT(D(4)), 2)
     D(4) = T * MIN(D(6), DAMP)
     IF (.NOT. (D(4) .LE. HUGE(ZERO))) GOTO 1
     IF (SSIZE .NE. 0) D(4) = -D(4)
     D(7) = MIN(D(7), DAMP)
     DO WHILE (CR_HYPOT(D(3), D(4)) .GT. T)
        D(3) = D(3) * D(7)
        D(4) = D(4) * D(7)
     END DO
     ES = 0_c_int
     T = CUTOFF
     SSIZE = INT(PVN_ZLJV2(D(1), D(2), D(3), D(4), D(5), D(6), D(7), T, ES))
     IF (SSIZE .LT. 0) THEN
        WRITE (ERROR_UNIT,'(I11,A,I3)') I, ': error', SSIZE
        GOTO 2
     END IF
     IF (CR_HYPOT(D(6), D(7)) .GE. (CUTOFF * D(5))) THEN
        ISEED(1) = ISEED(1) + 1
        IF (N .LT. 0) GOTO 1
     END IF
     Q(5) = D(5) ! CH
     Q(6) = D(6) ! SHR
     Q(7) = D(7) ! SHI
     Q(8) = CR_HYPOT(Q(6), Q(7))
     Q(8) = CR_HYPOT(Q(8), QONE)
     Q(8) = ABS((Q(5) - Q(8)) * (Q(5) + Q(8)))
     Q(1) = MAX(Q(1), Q(8))
     Q(11) = D(1)
     Q(12) = D(2)
     Q(13) = D(3)
     Q(14) = D(4)
     ES = 0_c_int
     W = CUTOFF
     SSIZE = INT(PVN_YLJV2(Q(11), Q(12), Q(13), Q(14), Q(8), Q(9), Q(10), W, ES))
     IF (SSIZE .LT. 0) THEN
        WRITE (ERROR_UNIT,'(I11,A,I3)') I, ': ERROR', SSIZE
        GOTO 2
     END IF
     Q(5) = ABS(Q(5) - Q(8)) / Q(8)
     Q(2) = MAX(Q(2), Q(5))
     Q(6) = ABS(Q(6) - Q(9))
     IF ((Q(9) .NE. QZERO) .OR. (Q(6) .NE. QZERO)) Q(6) = Q(6) / Q(9)
     Q(3) = MAX(Q(3), Q(6))
     Q(7) = ABS(Q(7) - Q(10))
     IF ((Q(10) .NE. QZERO) .OR. (Q(7) .NE. QZERO)) Q(7) = Q(7) / Q(10)
     Q(4) = MAX(Q(4), Q(7))
  END DO
  ! relative errors in the terms of \epsilon
  DO I = 1, 4
     Q(I) = Q(I) / DEPS
  END DO
  IF (N .LT. 0) THEN
     WRITE (OUTPUT_UNIT,'(I11,A,I11)',ADVANCE='NO') -ISEED(1), ',', -N
  ELSE ! N >= 0
     WRITE (OUTPUT_UNIT,'(I11,A,I11)',ADVANCE='NO')  ISEED(1), ',',  N
  END IF
  WRITE (OUTPUT_UNIT,'(4(A,ES25.17E3))') ',',&
       REAL(Q(1), K), ',', REAL(Q(2), K), ',', REAL(Q(3), K), ',', REAL(Q(4), K)
2 DEALLOCATE(ISEED)
END PROGRAM ZLJV2T

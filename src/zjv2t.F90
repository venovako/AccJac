PROGRAM ZJV2T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, REAL64, REAL128
  IMPLICIT NONE
  REAL(KIND=REAL128), PARAMETER :: QZERO = 0.0_REAL128, QONE = 1.0_REAL128
  REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64, HALF = 0.5_REAL64, CUTOFF = 0.8_REAL64
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=REAL128) :: Q(14)
  REAL(KIND=REAL64) :: D(7)
  INTEGER, ALLOCATABLE :: ISEED(:)
  !DIR$ ATTRIBUTES ALIGN: 64:: ISEED
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
  IF (N .LE. 0) STOP 'N <= 0'
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
  DO I = 1, SSIZE
     WRITE (ERROR_UNIT,*) ISEED(I)
  END DO
  Q = QZERO
  D = ZERO
  ISEED = 0
  DO I = 1, N
1    CALL RANDOM_NUMBER(D)
     D(1) = D(1) / D(3)
     IF (.NOT. (D(1) .LE. HUGE(ZERO))) GOTO 1
     D(2) = D(2) / D(4)
     IF (.NOT. (D(2) .LE. HUGE(ZERO))) GOTO 1
#ifdef ZJV2T_SAFE
     D(7) = D(7) * SQRT(HALF * D(1)) * SQRT(HALF * D(2))
#else
     D(7) = SQRT(HALF * D(1)) * SQRT(HALF * D(2))
#endif
     SSIZE = MOD(EXPONENT(D(3)), 2)
     D(3) = D(7) * D(5)
     IF (.NOT. (D(3) .LE. HUGE(ZERO))) GOTO 1
     IF (SSIZE .NE. 0) D(3) = -D(3)
     SSIZE = MOD(EXPONENT(D(4)), 2)
     D(4) = D(7) * D(6)
     IF (.NOT. (D(4) .LE. HUGE(ZERO))) GOTO 1
     IF (SSIZE .NE. 0) D(4) = -D(4)
     ES = 0_c_int
     SSIZE = INT(PVN_ZLJV2(D(1), D(2), D(3), D(4), D(5), D(6), D(7), ES))
     IF (SSIZE .LT. 0) THEN
        WRITE (ERROR_UNIT,'(I11,A,I3)') I, ': error', SSIZE
        GOTO 2
     END IF
     IF (HYPOT(D(6), D(7)) .GE. (CUTOFF * D(5))) ISEED(1) = ISEED(1) + 1
     Q(5) = D(5) ! CS
     Q(6) = D(6) ! SNR
     Q(7) = D(7) ! SNI
     Q(8) = HYPOT(Q(6), Q(7))
     Q(8) = HYPOT(Q(8), QONE)
     Q(8) = ABS((Q(5) - Q(8)) * (Q(5) + Q(8)))
     Q(1) = MAX(Q(1), Q(8))
     Q(11) = D(1)
     Q(12) = D(2)
     Q(13) = D(3)
     Q(14) = D(4)
     ES = 0_c_int
     SSIZE = INT(PVN_YLJV2(Q(11), Q(12), Q(13), Q(14), Q(8), Q(9), Q(10), ES))
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
  WRITE (OUTPUT_UNIT,'(I11,A,I11,4(A,ES25.17E3))') ISEED(1), ',', N, ',', Q(1), ',', Q(2), ',', Q(3), ',', Q(4)
2 DEALLOCATE(ISEED)
END PROGRAM ZJV2T

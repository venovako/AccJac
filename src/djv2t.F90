PROGRAM DJV2T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, REAL64, REAL128
  IMPLICIT NONE
  REAL(KIND=REAL128), PARAMETER :: QZERO = 0.0_REAL128, QONE = 1.0_REAL128
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=REAL128) :: Q(3)
  REAL(KIND=REAL64) :: D(5)
  INTEGER, ALLOCATABLE :: ISEED(:)
  !DIR$ ATTRIBUTES ALIGN: 64:: ISEED
  INTEGER :: I, N, SSIZE
  INTEGER(KIND=c_int) :: ES
  INTEGER(KIND=c_int), EXTERNAL :: PVN_DLJV2
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
  Q(3) = QZERO
  DO I = 1, N
     CALL RANDOM_NUMBER(D)
     SSIZE = MOD(EXPONENT(D(3)), 2)
     D(3) = SQRT(D(1)) * SQRT(D(2)) * D(3)
     IF (SSIZE .NE. 0) D(3) = -D(3)
     ES = 0_c_int
     SSIZE = INT(PVN_DLJV2(D(1), D(2), D(3), D(4), D(5), ES))
     IF (SSIZE .LT. 0) THEN
        WRITE (CLA,'(I11,A,I3)') I, ': ERROR', SSIZE
        STOP CLA
     ELSE ! OK
        Q(1) = D(4) ! CS
        Q(2) = D(5) ! SN
        Q(2) = HYPOT(Q(2), QONE)
        Q(2) = ABS((Q(1) - Q(2)) * (Q(1) + Q(2)))
        Q(3) = MAX(Q(3), Q(2))
     END IF
  END DO
  WRITE (OUTPUT_UNIT,'(ES26.17E4)') Q(3)
  DEALLOCATE(ISEED)
END PROGRAM DJV2T

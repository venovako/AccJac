! meant to be compiled with ifx and ABI=lp64;
! adjust the integer formats for ABI=ilp64
PROGRAM DTH2T
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, REAL64, REAL128
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION CR_RSQRT(X) BIND(C,NAME='cr_rsqrt')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       IMPLICIT NONE
       REAL(KIND=c_double), INTENT(IN), VALUE :: X
       REAL(KIND=c_double) :: CR_RSQRT
     END FUNCTION CR_RSQRT
  END INTERFACE
  REAL(KIND=REAL128), PARAMETER :: QZERO = 0.0_REAL128, QONE = 1.0_REAL128
  REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64, ONE = 1.0_REAL64
  REAL(KIND=REAL64), PARAMETER :: CUTOFF = 40.0_REAL64 / 41.0_REAL64, DEPS = EPSILON(ZERO) / 2
  INTEGER, PARAMETER :: ETH = 1, ECH = 2, ESH = 3, STH = 4, SCH = 5, SSH = 6, ARE = 1, MRE = 2
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=REAL128) :: Q(2,6), QD, QT, QC, QS, QE
  REAL(KIND=REAL64) :: D, T, C, S
  INTEGER, ALLOCATABLE :: ISEED(:)
  !DIR$ ATTRIBUTES ALIGN: 64:: ISEED
  INTEGER :: I, N, NEXP, SSIZE
  ! random seed may be given
  CALL RANDOM_SEED(SIZE=SSIZE)
  IF (SSIZE .LE. 0) STOP 'seed size non-positive'
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .LT. 2) THEN
     IF (SSIZE .GT. 1) THEN
        WRITE (CLA,'(I1)') SSIZE
        CLA = 'args: N EXP [SEED1 ... SEED'//TRIM(CLA)//']'
     ELSE ! SSIZE = 1
        CLA = 'args: N EXP [SEED1]'
     END IF
     WRITE (ERROR_UNIT,*) TRIM(CLA)
     STOP 'All SEED arguments have to be given, or none of them.'
  END IF
  CALL GET_COMMAND_ARGUMENT(2, CLA)
  READ (CLA,*) NEXP
  IF (NEXP .GE. 0) STOP 'EXP must be < 0'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) N
  IF (I .EQ. 2) THEN
     ALLOCATE(ISEED(SSIZE))
     CALL RANDOM_SEED
     CALL RANDOM_SEED(GET=ISEED)
  ELSE IF (I .EQ. (SSIZE + 2)) THEN
     ALLOCATE(ISEED(SSIZE))
     DO I = 1, SSIZE
        CALL GET_COMMAND_ARGUMENT(I + 2, CLA)
        READ (CLA,*) ISEED(I)
     END DO
     CALL RANDOM_SEED(PUT=ISEED)
  ELSE ! a wrong SEED
     STOP 'invalid number of SEED arguments'
  END IF
  WRITE (ERROR_UNIT,'(I11)',ADVANCE='NO') ISEED(1)
  DO I = 2, SSIZE-1
     WRITE (ERROR_UNIT,'(I12)',ADVANCE='NO') ISEED(I)
  END DO
  WRITE (ERROR_UNIT,'(I12)') ISEED(SSIZE)
  ISEED = 0
  Q = QZERO
  D = ZERO
  DO I = 1, ABS(N)
1    CALL RANDOM_NUMBER(D)
     D = FRACTION(D)
     SSIZE = NEXP + 1
     D = SCALE(D, SSIZE)
     IF (D .GE. CUTOFF) THEN
        ISEED(1) = ISEED(1) + 1
        IF (N .LT. 0) GOTO 1
     END IF
     ! "enhanced" formulas
     T = D / (ONE + SQRT(IEEE_FMA(-D, D, ONE)))
     C = CR_RSQRT(IEEE_FMA(-T, T, ONE))
     S = C * T
     ! error check
     QD = D
     QT = QD / (QONE + SQRT(IEEE_FMA(-QD, QD, QONE)))
     QC = QONE / SQRT(IEEE_FMA(-QT, QT, QONE))
     QS = QC * QT
     QD = I - 1
     QD = QD / I
     QE = T
     QE = ABS(QT - QE) / QT
     Q(ARE,ETH) = IEEE_FMA(Q(ARE,ETH), QD, QE / I)
     Q(MRE,ETH) = MAX(Q(MRE,ETH), QE)
     QE = C
     QE = ABS(QC - QE) / QC
     Q(ARE,ECH) = IEEE_FMA(Q(ARE,ECH), QD, QE / I)
     Q(MRE,ECH) = MAX(Q(MRE,ECH), QE)
     QE = S
     QE = ABS(QS - QE) / QS
     Q(ARE,ESH) = IEEE_FMA(Q(ARE,ESH), QD, QE / I)
     Q(MRE,ESH) = MAX(Q(MRE,ESH), QE)
     ! "standard" formulas
     T = D / (ONE + SQRT((ONE - D) * (ONE + D)))
     C = ONE / SQRT((ONE - T) * (ONE + T))
     S = C * T
     ! error check
     QE = T
     QE = ABS(QT - QE) / QT
     Q(ARE,STH) = IEEE_FMA(Q(ARE,STH), QD, QE / I)
     Q(MRE,STH) = MAX(Q(MRE,STH), QE)
     QE = C
     QE = ABS(QC - QE) / QC
     Q(ARE,SCH) = IEEE_FMA(Q(ARE,SCH), QD, QE / I)
     Q(MRE,SCH) = MAX(Q(MRE,SCH), QE)
     QE = S
     QE = ABS(QS - QE) / QS
     Q(ARE,SSH) = IEEE_FMA(Q(ARE,SSH), QD, QE / I)
     Q(MRE,SSH) = MAX(Q(MRE,SSH), QE)
  END DO
#ifndef NDEBUG
  IF (N .LT. 0) WRITE (ERROR_UNIT,'(A,I11)') 'Skipped:', ISEED(1)
#endif
  DEALLOCATE(ISEED)
  ! relative errors in the terms of \epsilon
  QE = DEPS
  Q = Q / QE
  WRITE (OUTPUT_UNIT,'(I3,A,I11,12(A,ES25.17E3))') NEXP, ',', ABS(N), ',', &
       Q(ARE,ETH), ',', Q(MRE,ETH), ',', Q(ARE,ECH), ',', Q(MRE,ECH), ',', Q(ARE,ESH), ',', Q(MRE,ESH), ',', &
       Q(ARE,STH), ',', Q(MRE,STH), ',', Q(ARE,SCH), ',', Q(MRE,SCH), ',', Q(ARE,SSH), ',', Q(MRE,SSH)
END PROGRAM DTH2T

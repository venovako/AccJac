PROGRAM STH2T
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, REAL32, REAL128
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION CR_RSQRT(X) BIND(C,NAME='cr_rsqrtf')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float
       IMPLICIT NONE
       REAL(KIND=c_float), INTENT(IN), VALUE :: X
       REAL(KIND=c_float) :: CR_RSQRT
     END FUNCTION CR_RSQRT
  END INTERFACE
  REAL(KIND=REAL128), PARAMETER :: QZERO = 0.0_REAL128, QONE = 1.0_REAL128
  REAL(KIND=REAL32), PARAMETER :: ZERO = 0.0_REAL32, ONE = 1.0_REAL32
  REAL(KIND=REAL32), PARAMETER :: CUTOFF = 40.0_REAL32 / 41.0_REAL32, DEPS = EPSILON(ZERO) / 2
  INTEGER, PARAMETER :: ETH = 1, ECH = 2, ESH = 3, ARE = 1, MRE = 2
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=REAL128) :: Q(2,3), QD, QT, QC, QS, QE
  REAL(KIND=REAL32) :: D, T, C, S
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
  DO I = 1, SSIZE-1
     WRITE (ERROR_UNIT,'(I12)',ADVANCE='NO') ISEED(I)
  END DO
  WRITE (ERROR_UNIT,'(I12)') ISEED(SSIZE)
  ISEED = 0
  Q = QZERO
  D = ZERO
  DO I = 1, ABS(N)
1    CALL RANDOM_NUMBER(D)
     IF (.NOT. (D .GE. TINY(ZERO))) GOTO 1
     D = FRACTION(D)
     SSIZE = NEXP + 1
     D = SCALE(D, SSIZE)
     IF (D .GE. CUTOFF) THEN
        ISEED(1) = ISEED(1) + 1
        IF (N .LT. 0) GOTO 1
     END IF
     T = D / (ONE + SQRT(IEEE_FMA(-D, D, ONE)))
     C = CR_RSQRT(IEEE_FMA(-T, T, ONE))
     S = C * T
     QD = D
     QT = QD / (QONE + SQRT(IEEE_FMA(-QD, QD, QONE)))
     QS = SQRT(IEEE_FMA(-QT, QT, QONE))
     QC = QONE / QS
     QS = QT / QS
     QD = I - 1
     QD = QD / ABS(N)
     QE = T
     QE = ABS(QT - QE) / QT
     Q(ARE,ETH) = Q(ARE,ETH) * QD + QE / I
     Q(MRE,ETH) = MAX(Q(MRE,ETH), QE)
     QE = C
     QE = ABS(QC - QE) / QC
     Q(ARE,ECH) = Q(ARE,ECH) * QD + QE / I
     Q(MRE,ECH) = MAX(Q(MRE,ECH), QE)
     QE = S
     QE = ABS(QS - QE) / QS
     Q(ARE,ESH) = Q(ARE,ESH) * QD + QE / I
     Q(MRE,ESH) = MAX(Q(MRE,ESH), QE)
  END DO
#ifndef NDEBUG
  IF (N .LT. 0) WRITE (ERROR_UNIT,'(A,I11)') 'Skipped:', ISEED(1)
#endif
  DEALLOCATE(ISEED)
  ! relative errors in the terms of \epsilon
  QE = DEPS
  Q = Q / DEPS
#ifndef NDEBUG
  WRITE (OUTPUT_UNIT,'(A)') '"EXP(TH(2φ))", "N", "AVG(ρ(TH))/ε", "MAX(ρ(TH))/ε", "AVG(ρ(CH))/ε", "MAX(ρ(CH))/ε", "AVG(ρ(SH))/ε", "MAX(ρ(SH))/ε"'
#endif
  WRITE (OUTPUT_UNIT,'(I3,A,I11,6(A,ES16.9E2))') NEXP, ',', ABS(N), &
       ',', Q(ARE,ETH), ',', Q(MRE,ETH), &
       ',', Q(ARE,ECH), ',', Q(MRE,ECH), &
       ',', Q(ARE,ESH), ',', Q(MRE,ESH)
END PROGRAM STH2T

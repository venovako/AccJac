!!! QUADRUPLE PRECISION BEHAVES DIFFERENTLY WITH THE GNU FORTRAN AND WITH THE INTEL FORTRAN !!!
PROGRAM STH2T
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, INT32, REAL32, REAL64
  IMPLICIT NONE
  REAL(KIND=REAL32), PARAMETER :: ZERO = 0.0_REAL32, ONE = 1.0_REAL32, CUTOFF = 40.0_REAL32 / 41.0_REAL32
  REAL(KIND=REAL64), PARAMETER :: DZERO = 0.0_REAL64, DEPS = EPSILON(ZERO) / 2.0
  INTEGER, PARAMETER :: ETH = 1, ECH = 2, ESH = 3, STH = 4, SCH = 5, SSH = 6, ARE = 1, MRE = 2
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=REAL64) :: Q(2,6), QD, QT, QC, QS, QE
  REAL(KIND=REAL32) :: D, T, C, S, U
  INTEGER :: I, NEXP
  INTEGER(KIND=INT32) :: ID
  EQUIVALENCE (D,ID)
  EXTERNAL :: MPFR_START, MPFR_STOP, MPFR_TCS, MPFR_RE, TCS_OLD, TCS_NEW
  IF (COMMAND_ARGUMENT_COUNT() .NE. 1) STOP 'sth2t.exe EXP'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) NEXP
  IF (NEXP .GE. 0) STOP 'EXP must be < 0'
  Q = DZERO
  IF (NEXP .EQ. -1) THEN
     U = CUTOFF
  ELSE ! NEXP < -1
     U = SCALE(ONE, NEXP + 1)
  END IF
  D = SCALE(ONE, NEXP)
  I = 1024
  CALL MPFR_START(I)
  IF (I .NE. 0) STOP 'MPFR_START'
  I = 1
  DO WHILE (D .LT. U)
     CALL TCS_NEW(D, T, C, S)
     ! error check
     QD = D
     CALL MPFR_TCS(QD, QT, QC, QS)
     QD = I - 1
     QD = QD / I
     QE = T
     CALL MPFR_RE(QT, QE, DEPS)
     !DIR$ FMA
     Q(ARE,ETH) = Q(ARE,ETH) * QD + (QE / I)
     Q(MRE,ETH) = MAX(Q(MRE,ETH), QE)
     QE = C
     CALL MPFR_RE(QC, QE, DEPS)
     !DIR$ FMA
     Q(ARE,ECH) = Q(ARE,ECH) * QD + (QE / I)
     Q(MRE,ECH) = MAX(Q(MRE,ECH), QE)
     QE = S
     CALL MPFR_RE(QS, QE, DEPS)
     !DIR$ FMA
     Q(ARE,ESH) = Q(ARE,ESH) * QD + (QE / I)
     Q(MRE,ESH) = MAX(Q(MRE,ESH), QE)
     ! "standard" formulas
     CALL TCS_OLD(D, T, C, S);
     ! error check
     QE = T
     CALL MPFR_RE(QT, QE, DEPS)
     !DIR$ FMA
     Q(ARE,STH) = Q(ARE,STH) * QD + (QE / I)
     Q(MRE,STH) = MAX(Q(MRE,STH), QE)
     QE = C
     CALL MPFR_RE(QC, QE, DEPS)
     !DIR$ FMA
     Q(ARE,SCH) = Q(ARE,SCH) * QD + (QE / I)
     Q(MRE,SCH) = MAX(Q(MRE,SCH), QE)
     QE = S
     CALL MPFR_RE(QS, QE, DEPS)
     !DIR$ FMA
     Q(ARE,SSH) = Q(ARE,SSH) * QD + (QE / I)
     Q(MRE,SSH) = MAX(Q(MRE,SSH), QE)
     ! increment
     ID = ID + 1
     I = I + 1
  END DO
  CALL MPFR_STOP()
  ! relative errors in the terms of \epsilon
  I = I - 1
  WRITE (OUTPUT_UNIT,'(I3,A,I11,12(A,ES16.9E2))') NEXP, ',', I, ',',&
       Q(ARE,ETH), ',', Q(MRE,ETH), ',', Q(ARE,ECH), ',', Q(MRE,ECH), ',',&
       Q(ARE,ESH), ',', Q(MRE,ESH), ',', Q(ARE,STH), ',', Q(MRE,STH), ',',&
       Q(ARE,SCH), ',', Q(MRE,SCH), ',', Q(ARE,SSH), ',', Q(MRE,SSH)
END PROGRAM STH2T

PROGRAM STH2T
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, INT32, REAL32, REAL128
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION CR_RSQRTF(X) BIND(C,NAME='cr_rsqrtf')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float
       IMPLICIT NONE
       REAL(KIND=c_float), INTENT(IN), VALUE :: X
       REAL(KIND=c_float) :: CR_RSQRTF
     END FUNCTION CR_RSQRTF
  END INTERFACE
  INTERFACE
     PURE FUNCTION CR_RSQRTQ(X) BIND(C,NAME='cr_rsqrtq')
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       REAL(KIND=REAL128), INTENT(IN), VALUE :: X
       REAL(KIND=REAL128) :: CR_RSQRTQ
     END FUNCTION CR_RSQRTQ
  END INTERFACE
  REAL(KIND=REAL128), PARAMETER :: QZERO = 0.0_REAL128, QONE = 1.0_REAL128
  REAL(KIND=REAL32), PARAMETER :: ZERO = 0.0_REAL32, ONE = 1.0_REAL32
  REAL(KIND=REAL32), PARAMETER :: CUTOFF = 40.0_REAL32 / 41.0_REAL32, DEPS = EPSILON(ZERO) / 2
  INTEGER, PARAMETER :: ETH = 1, ECH = 2, ESH = 3, STH = 4, SCH = 5, SSH = 6, ARE = 1, MRE = 2
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=REAL128) :: Q(2,6), QD, QT, QC, QS, QE
  REAL(KIND=REAL32) :: D, T, C, S, U
  INTEGER :: I, NEXP
  INTEGER(KIND=INT32) :: ID
  EQUIVALENCE (D,ID)
  IF (COMMAND_ARGUMENT_COUNT() .NE. 1) STOP 'sth2t.exe EXP'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) NEXP
  IF (NEXP .GE. 0) STOP 'EXP must be < 0'
  Q = QZERO
  IF (NEXP .EQ. -1) THEN
     U = CUTOFF
  ELSE ! NEXP < -1
     U = SCALE(ONE, NEXP + 1)
  END IF
  D = SCALE(ONE, NEXP)
  I = 1
  DO WHILE (D .LT. U)
     ! "enhanced" formulas
     T = D / (ONE + SQRT(IEEE_FMA(-D, D, ONE)))
     C = CR_RSQRTF(IEEE_FMA(-T, T, ONE))
     S = C * T
     ! error check
     QD = D
     QT = QD / (QONE + SQRT(IEEE_FMA(-QD, QD, QONE)))
     QC = CR_RSQRTQ(IEEE_FMA(-QT, QT, QONE))
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
     ! increment
     ID = ID + 1
     I = I + 1
  END DO
  ! relative errors in the terms of \epsilon
  QE = DEPS
  Q = Q / QE
  I = I - 1
  WRITE (OUTPUT_UNIT,'(I3,A,I11,12(A,ES16.9E2))') NEXP, ',', I, ',', &
       Q(ARE,ETH), ',', Q(MRE,ETH), ',', Q(ARE,ECH), ',', Q(MRE,ECH), ',', Q(ARE,ESH), ',', Q(MRE,ESH), ',', &
       Q(ARE,STH), ',', Q(MRE,STH), ',', Q(ARE,SCH), ',', Q(MRE,SCH), ',', Q(ARE,SSH), ',', Q(MRE,SSH)
END PROGRAM STH2T

!!! QUADRUPLE PRECISION BEHAVES DIFFERENTLY WITH THE GNU FORTRAN AND WITH THE INTEL FORTRAN !!!
! MPFR is used instead of REAL128 !
PROGRAM STH2T
  USE MPFR_F
  IMPLICIT NONE
#include "cr.f90"
  INTEGER, PARAMETER :: K = REAL32, KK = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, TWO = 2.0_K
  REAL(KIND=K), PARAMETER :: CUTOFF2 = 40.0_K / 41.0_K, EPS = EPSILON(ZERO) / TWO
  REAL(KIND=KK), PARAMETER :: DZERO = 0.0_KK
  INTEGER, PARAMETER :: ETH = 1, ECH = 2, ESH = 3, STH = 4, SCH = 5, SSH = 6, DET = 7, ARE = 1, MRE = 2
  TYPE(MPFR_T) :: MO, MD, MT, MC, MS, ME, MF, MG
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=KK) :: Q(2,7), QD, QT, QC, QS, J
  REAL(KIND=K) :: D, T, C, S, U
  INTEGER :: I, NEXP
  INTEGER(KIND=INT32) :: ID
  EQUIVALENCE (D,ID)
  EXTERNAL :: TCS_OLD, TCS_NEW
  IF (COMMAND_ARGUMENT_COUNT() .NE. 1) STOP 'sth2t.exe EXP'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) NEXP
  IF (NEXP .GE. 0) STOP 'EXP must be < 0'
  Q = DZERO
  IF (NEXP .EQ. -1) THEN
     U = CUTOFF2
  ELSE ! NEXP < -1
     U = SCALE(ONE, NEXP + 1)
  END IF
  D = SCALE(ONE, NEXP)
  I = 1024
  IF (MPFR_INITIALIZE(INT(I, MPFR_PREC_KIND)) .NE. 0) STOP 'MPFR_INITIALIZE'
  CALL MPFR_INIT_M(MO)
  MO = ONE
  CALL MPFR_INIT_M(MD)
  CALL MPFR_SET_ZERO(MD, MD%TAG)
  CALL MPFR_INIT_M(MT)
  CALL MPFR_SET_ZERO(MT, MT%TAG)
  CALL MPFR_INIT_M(MC)
  CALL MPFR_SET_ZERO(MC, MC%TAG)
  CALL MPFR_INIT_M(MS)
  CALL MPFR_SET_ZERO(MS, MS%TAG)
  CALL MPFR_INIT_M(ME)
  ME = EPS
  CALL MPFR_INIT_M(MF)
  CALL MPFR_SET_ZERO(MF, MF%TAG)
  CALL MPFR_INIT_M(MG)
  CALL MPFR_SET_ZERO(MG, MG%TAG)
  I = 1
  DO WHILE (D .LT. U)
     T = D / (ONE + SQRT(SFMA(-D, D, ONE)))
     C = CR_RSQRT(SFMA(-T, T, ONE))
     S = C * T
     ! new formulas
     MD = D
     ! T
     CALL MPFR_NEG_F(MC, MD)
     CALL MPFR_FMA_F(MS, MC, MD, MO)
     CALL MPFR_SQRT_F(MS, MS)
     CALL MPFR_ADD_F(MS, MS, MO)
     CALL MPFR_DIV_F(MT, MD, MS)
     ! C
     CALL MPFR_NEG_F(MC, MT)
     CALL MPFR_FMA_F(MS, MC, MT, MO)
     CALL MPFR_REC_SQRT_F(MC, MS)
     ! S
     CALL MPFR_MUL_F(MS, MC, MT)
     ! relerr det = 1 - (c^2 - s^2) = 1 + (s - c)*(s + c)
     MF = C
     MG = S
     CALL MPFR_SUB_F(MD, MG, MF)
     CALL MPFR_ADD_F(MF, MG, MF)
     CALL MPFR_FMA_F(MD, MD, MF, MO)
     CALL MPFR_ABS_F(MD, MD)
     CALL MPFR_DIV_F(MD, MD, ME)
     J = MD
     IF (T .GE. 0.5) WRITE (ERROR_UNIT,'(ES16.9E2,A,ES16.9E2)') T, ',', J
     ! relerr T
     MD = T
     CALL MPFR_SUB_F(MD, MT, MD)
     CALL MPFR_ABS_F(MD, MD)
     CALL MPFR_MUL_F(MF, ME, MT)
     CALL MPFR_DIV_F(MD, MD, MF)
     QT = MD
     ! relerr C
     MD = C
     CALL MPFR_SUB_F(MD, MC, MD)
     CALL MPFR_ABS_F(MD, MD)
     CALL MPFR_MUL_F(MF, ME, MC)
     CALL MPFR_DIV_F(MD, MD, MF)
     QC = MD
     ! relerr S
     MD = S
     CALL MPFR_SUB_F(MD, MS, MD)
     CALL MPFR_ABS_F(MD, MD)
     CALL MPFR_MUL_F(MF, ME, MS)
     CALL MPFR_DIV_F(MD, MD, MF)
     QS = MD
     ! stat
     QD = I - 1
     QD = QD / I
     Q(ARE,ETH) = DFMA(Q(ARE,ETH), QD, (QT / I))
     Q(MRE,ETH) = MAX(Q(MRE,ETH), QT)
     Q(ARE,ECH) = DFMA(Q(ARE,ECH), QD, (QC / I))
     Q(MRE,ECH) = MAX(Q(MRE,ECH), QC)
     Q(ARE,ESH) = DFMA(Q(ARE,ESH), QD, (QS / I))
     Q(MRE,ESH) = MAX(Q(MRE,ESH), QS)
     Q(ARE,DET) = DFMA(Q(ARE,DET), QD, (J / I))
     Q(MRE,DET) = MAX(Q(MRE,DET), J)
     ! "standard" formulas
     C = ONE - D
     S = ONE + D
     T = D / (ONE + SQRT(C * S))
     C = ONE - T
     S = ONE + T
     C = ONE / SQRT(C * S)
     S = C * T
     ! relerr T
     MD = T
     CALL MPFR_SUB_F(MD, MT, MD)
     CALL MPFR_ABS_F(MD, MD)
     CALL MPFR_MUL_F(MF, ME, MT)
     CALL MPFR_DIV_F(MD, MD, MF)
     QT = MD
     ! relerr C
     MD = C
     CALL MPFR_SUB_F(MD, MC, MD)
     CALL MPFR_ABS_F(MD, MD)
     CALL MPFR_MUL_F(MF, ME, MC)
     CALL MPFR_DIV_F(MD, MD, MF)
     QC = MD
     ! relerr S
     MD = S
     CALL MPFR_SUB_F(MD, MS, MD)
     CALL MPFR_ABS_F(MD, MD)
     CALL MPFR_MUL_F(MF, ME, MS)
     CALL MPFR_DIV_F(MD, MD, MF)
     QS = MD
     ! stat
     Q(ARE,STH) = DFMA(Q(ARE,STH), QD, (QT / I))
     Q(MRE,STH) = MAX(Q(MRE,STH), QT)
     Q(ARE,SCH) = DFMA(Q(ARE,SCH), QD, (QC / I))
     Q(MRE,SCH) = MAX(Q(MRE,SCH), QC)
     Q(ARE,SSH) = DFMA(Q(ARE,SSH), QD, (QS / I))
     Q(MRE,SSH) = MAX(Q(MRE,SSH), QS)
     ! increment
     ID = ID + 1
     I = I + 1
  END DO
  CALL MPFR_CLEAR_M(MG)
  CALL MPFR_CLEAR_M(MF)
  CALL MPFR_CLEAR_M(ME)
  CALL MPFR_CLEAR_M(MS)
  CALL MPFR_CLEAR_M(MC)
  CALL MPFR_CLEAR_M(MT)
  CALL MPFR_CLEAR_M(MD)
  CALL MPFR_CLEAR_M(MO)
  CALL MPFR_FINALIZE()
  ! relative errors in the terms of \epsilon
  I = I - 1
  WRITE (OUTPUT_UNIT,'(I3,A,I11,14(A,ES16.9E2))') NEXP, ',', I, ',',&
       Q(ARE,ETH), ',', Q(MRE,ETH), ',', Q(ARE,ECH), ',', Q(MRE,ECH), ',',&
       Q(ARE,ESH), ',', Q(MRE,ESH), ',', Q(ARE,STH), ',', Q(MRE,STH), ',',&
       Q(ARE,SCH), ',', Q(MRE,SCH), ',', Q(ARE,SSH), ',', Q(MRE,SSH), ',',&
       Q(ARE,DET), ',', Q(MRE,DET)
END PROGRAM STH2T

! determinants only
PROGRAM YTEST
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, PARAMETER :: CLAL = 256
  REAL(REAL128), PARAMETER :: HALF = 0.5_REAL128, ZERO = 0.0_REAL128, ONE = 1.0_REAL128
  CHARACTER(LEN=CLAL) :: CLA
  COMPLEX(REAL128) :: A, B, C, SN1, QSN1
  REAL(REAL128) :: RT1, RT2, CS1, QCS1, QJD, QLD, MJD, XJD, MLD, XLD, E_2
  INTEGER :: I, N, U, INFO
  EXTERNAL :: YJIEV2, YLAEV2
#ifdef MPFR
  REAL(REAL128), EXTERNAL :: MDETM1
  EXTERNAL :: INIT_MPFR, FINI_MPFR
#else
#define MDETM1 YDETM1
#endif
  I = CLAL
  CALL GET_COMMAND_ARGUMENT(0, CLA, I, INFO)
  IF (INFO .NE. 0) STOP 'argv[0]'
  CLA = TRIM(CLA)//' N'
  IF (COMMAND_ARGUMENT_COUNT() .NE. 1) STOP CLA
  I = CLAL
  CALL GET_COMMAND_ARGUMENT(1, CLA, I, INFO)
  IF (INFO .NE. 0) STOP 'argv[1]'
  READ (CLA,*) N
  IF (N .LE. 0) STOP 'N < 0'
  E_2 = ZERO
  E_2 = ONE / E_2
  MJD = E_2
  MLD = E_2
  E_2 = -E_2
  XJD = E_2
  XLD = E_2
  E_2 = EPSILON(HALF) * HALF
  U = ORFILE()
#ifdef MPFR
  CALL INIT_MPFR(INFO)
  IF (INFO .NE. 0) STOP 'INIT_MPFR'
#endif
  I = 1
  DO WHILE (I .LE. N)
     A = QRSAFE(U)
     B = CMPLX(QRSAFE(U), QRSAFE(U), REAL128)
     C = QRSAFE(U)
#ifndef NDEBUG
     WRITE (*,9,ADVANCE='NO') 'A(1,:):(', REAL(A)
     WRITE (*,9,ADVANCE='NO') ',', AIMAG(A)
     WRITE (*,9,ADVANCE='NO') ') (', REAL(B)
     WRITE (*,9,ADVANCE='NO') ',', AIMAG(B)
     WRITE (*,'(A)') ')'
     WRITE (*,9,ADVANCE='NO') 'A(2,:):(', REAL(B)
     WRITE (*,9,ADVANCE='NO') ',', -AIMAG(B)
     WRITE (*,9,ADVANCE='NO') ') (', REAL(C)
     WRITE (*,9,ADVANCE='NO') ',', AIMAG(C)
     WRITE (*,'(A)') ')'
#endif
     INFO = 0
     CALL YJIEV2(A, B, C, RT1, RT2, CS1, SN1, INFO)
     IF (INFO .NE. 0) THEN
#ifndef NDEBUG
        WRITE (*,'(A,I2)') 'YJIEV2=', INFO
#endif
        CYCLE
     END IF
     QCS1 = CS1
     QSN1 = SN1
#ifndef NDEBUG
     WRITE (*,9) 'QCS1=', CS1
     WRITE (*,9,ADVANCE='NO') 'QSN1=(', REAL(SN1)
     WRITE (*,9,ADVANCE='NO') ',', AIMAG(SN1)
     WRITE (*,'(A)') ')'
     WRITE (*,9) 'QRT1=', RT1
     WRITE (*,9) 'QRT2=', RT2
#endif
     QJD = MDETM1(CS1, SN1, E_2)
     IF (QJD .LT. MJD) MJD = QJD
     IF (QJD .GT. XJD) XJD = QJD
#ifndef NDEBUG
     WRITE (*,9) ' QJD=', QJD
#endif
     CALL YLAEV2(A, B, C, RT1, RT2, CS1, SN1)
#ifndef NDEBUG
     WRITE (*,9) 'LCS1=', CS1
     WRITE (*,9,ADVANCE='NO') 'LSN1=(', REAL(SN1)
     WRITE (*,9,ADVANCE='NO') ',', AIMAG(SN1)
     WRITE (*,'(A)') ')'
     WRITE (*,9) 'LRT1=', RT1
     WRITE (*,9) 'LRT2=', RT2
#endif
     QLD = MDETM1(CS1, SN1, E_2)
     IF (QLD .LT. MLD) MLD = QLD
     IF (QLD .GT. XLD) XLD = QLD
#ifndef NDEBUG
     WRITE (*,9) ' QLD=', QLD
#endif
     I = I + 1
  END DO
#ifdef MPFR
  CALL FINI_MPFR()
#endif
  CLOSE(U)
  WRITE (*,9) 'YJAEV2:min((det(U)-1)/ε)=', MJD
  WRITE (*,9) 'YJAEV2:max((det(U)-1)/ε)=', XJD
  WRITE (*,9) 'YLAEV2:min((det(U)-1)/ε)=', MLD
  WRITE (*,9) 'YLAEV2:max((det(U)-1)/ε)=', XLD
9 FORMAT(A,ES45.36E4)
CONTAINS
#include "orfile.F90"
#include "qrsafe.F90"
#ifndef MPFR
#include "ydetm1.F90"
#endif
END PROGRAM YTEST

PROGRAM WTEST
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, PARAMETER :: CLAL = 256
  REAL(c_long_double), PARAMETER :: HALF = 0.5_c_long_double
  REAL(REAL128), PARAMETER :: QZERO = 0.0_REAL128, QONE = 1.0_REAL128
  CHARACTER(LEN=CLAL) :: CLA
  COMPLEX(c_long_double), TARGET :: A, B, C, SN1
  REAL(c_long_double), TARGET :: RT1, RT2, CS1
  COMPLEX(REAL128), TARGET :: QA, QB, QC, QSN1
  REAL(REAL128), TARGET :: QRT1, QRT2, QCS1
  REAL(REAL128) :: QJD, QREC, QRESR, QRESI, MJD, XJD, MREC, XREC, MRESR, XRESR, MRESI, XRESI, E_2
  INTEGER :: I, N, U, INFO
  EXTERNAL :: XJIEV2, YJIEV2

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
  E_2 = QZERO
  E_2 = QONE / E_2
  MJD = E_2
  MREC = E_2
  MRESR = E_2
  MRESI = E_2
  E_2 = -E_2
  XJD = E_2
  XREC = E_2
  XRESR = E_2
  XRESI = E_2
  E_2 = EPSILON(HALF) * HALF
  U = ORFILE()
  I = 1
  DO WHILE (I .LE. N)
     A = XRSAFE(U)
     B = CMPLX(XRSAFE(U), XRSAFE(U), c_long_double)
     C = XRSAFE(U)
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
     CALL XJIEV2(A, B, C, RT1, RT2, CS1, SN1, INFO)
     IF (INFO .NE. 0) THEN
#ifndef NDEBUG
        WRITE (*,'(A,I2)') 'XJIEV2=', INFO
#endif
        CYCLE
     END IF
#ifndef NDEBUG
     WRITE (*,9) ' CS1=', CS1
     WRITE (*,9,ADVANCE='NO') ' SN1=(', REAL(SN1)
     WRITE (*,9,ADVANCE='NO') ',', AIMAG(SN1)
     WRITE (*,'(A)') ')'
     WRITE (*,9) ' RT1=', RT1
     WRITE (*,9) ' RT2=', RT2
#endif
     QA = A
     QB = B
     QC = C
     INFO = 0
     CALL YJIEV2(QA, QB, QC, QRT1, QRT2, QCS1, QSN1, INFO)
     IF (INFO .NE. 0) THEN
#ifndef NDEBUG
        WRITE (*,'(A,I2)') 'YJIEV2=', INFO
#endif
        CYCLE
     END IF
#ifndef NDEBUG
     WRITE (*,9) 'QCS1=', QCS1
     WRITE (*,9,ADVANCE='NO') 'QSN1=(', REAL(QSN1)
     WRITE (*,9,ADVANCE='NO') ',', AIMAG(QSN1)
     WRITE (*,'(A)') ')'
     WRITE (*,9) 'QRT1=', QRT1
     WRITE (*,9) 'QRT2=', QRT2
#endif
     QREC = CS1
     QRESR = QCS1
     QREC = QRE(QREC, QRESR, E_2)
     IF (QREC .LT. MREC) MREC = QREC
     IF (QREC .GT. XREC) XREC = QREC
#ifndef NDEBUG
     WRITE (*,9) 'QREC=', QREC
#endif
     QREC = REAL(SN1)
     QRESR = REAL(QSN1)
     QRESR = QRE(QREC, QRESR, E_2)
     IF (QRESR .LT. MRESR) MRESR = QRESR
     IF (QRESR .GT. XRESR) XRESR = QRESR
     QREC = AIMAG(SN1)
     QRESI = AIMAG(QSN1)
     QRESI = QRE(QREC, QRESI, E_2)
     IF (QRESI .LT. MRESI) MRESI = QRESI
     IF (QRESI .GT. XRESI) XRESI = QRESI
#ifndef NDEBUG
     WRITE (*,9,ADVANCE='NO') 'QRES=(', QRESR
     WRITE (*,9,ADVANCE='NO') ',', QRESI
     WRITE (*,'(A)') ')'
#endif
     QCS1 = CS1
     QSN1 = SN1
     QJD = YDETM1(QCS1, QSN1, E_2)
     IF (QJD .LT. MJD) MJD = QJD
     IF (QJD .GT. XJD) XJD = QJD
#ifndef NDEBUG
     WRITE (*,9) ' QJD=', QJD
#endif
     I = I + 1
  END DO
  CLOSE(U)
  WRITE (*,9) 'ZJAEV2:min((det(U)-1)/ε)=', MJD
  WRITE (*,9) 'ZJAEV2:max((det(U)-1)/ε)=', XJD
  WRITE (*,9) '     min(relerr(cosφ)/ε)=', MREC
  WRITE (*,9) '     max(relerr(cosφ)/ε)=', XREC
  WRITE (*,9) 'min(relerr(cosα*sinφ)/ε)=', MRESR
  WRITE (*,9) 'max(relerr(cosα*sinφ)/ε)=', XRESR
  WRITE (*,9) 'min(relerr(sinα*sinφ)/ε)=', MRESI
  WRITE (*,9) 'max(relerr(sinα*sinφ)/ε)=', XRESI
9 FORMAT(A,ES30.21E4)
CONTAINS
#include "orfile.F90"
#include "xrsafe.F90"
#include "ydetm1.F90"
#include "qre.F90"
END PROGRAM WTEST

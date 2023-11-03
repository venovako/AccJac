PROGRAM XTEST
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, PARAMETER :: CLAL = 256
  REAL(c_long_double), PARAMETER :: HALF = 0.5_c_long_double
  REAL(REAL128), PARAMETER :: QZERO = 0.0_REAL128, QONE = 1.0_REAL128
  CHARACTER(LEN=CLAL) :: CLA
  REAL(c_long_double), TARGET :: A, B, C, RT1, RT2, CS1, SN1
  REAL(REAL128), TARGET :: QA, QB, QC, QRT1, QRT2, QCS1, QSN1
  REAL(REAL128) :: QJD, QREC, QRES, MJD, XJD, MREC, XREC, MRES, XRES, E_2
  INTEGER :: I, N, U, INFO
  EXTERNAL :: XJIEV2, QJIEV2

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
  MRES = E_2
  E_2 = -E_2
  XJD = E_2
  XREC = E_2
  XRES = E_2
  E_2 = EPSILON(HALF) * HALF
  U = ORFILE()
  I = 1
  DO WHILE (I .LE. N)
     A = XRSAFE(U)
     B = XRSAFE(U)
     C = XRSAFE(U)
#ifndef NDEBUG
     WRITE (*,9,ADVANCE='NO') 'A(1,:):', A
     WRITE (*,9) ',', B
     WRITE (*,9,ADVANCE='NO') 'A(2,:):', B
     WRITE (*,9) ',', C
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
     WRITE (*,9) ' SN1=', SN1
     WRITE (*,9) ' RT1=', RT1
     WRITE (*,9) ' RT2=', RT2
#endif
     QA = A
     QB = B
     QC = C
     INFO = 0
     CALL QJIEV2(QA, QB, QC, QRT1, QRT2, QCS1, QSN1, INFO)
     IF (INFO .NE. 0) THEN
#ifndef NDEBUG
        WRITE (*,'(A,I2)') 'QJIEV2=', INFO
#endif
        CYCLE
     END IF
#ifndef NDEBUG
     WRITE (*,9) 'QCS1=', QCS1
     WRITE (*,9) 'QSN1=', QSN1
     WRITE (*,9) 'QRT1=', QRT1
     WRITE (*,9) 'QRT2=', QRT2
#endif
     QREC = CS1
     QRES = QCS1
     QREC = QRE(QREC, QRES, E_2)
#ifndef NDEBUG
     WRITE (*,9) 'QREC=', QREC
#endif
     IF (QREC .LT. MREC) MREC = QREC
     IF (QREC .GT. XREC) XREC = QREC
     QREC = SN1
     QRES = QSN1
     QRES = QRE(QREC, QRES, E_2)
#ifndef NDEBUG
     WRITE (*,9) 'QRES=', QRES
#endif
     IF (QRES .LT. MRES) MRES = QRES
     IF (QRES .GT. XRES) XRES = QRES
     QREC = CS1
     QRES = SN1
     QJD = QDETM1(QREC, QRES, E_2)
#ifndef NDEBUG
     WRITE (*,9) ' QJD=', QJD
#endif
     IF (QJD .LT. MJD) MJD = QJD
     IF (QJD .GT. XJD) XJD = QJD
     I = I + 1
  END DO
  CLOSE(U)
  WRITE (*,9) 'XJAEV2:min((det(U)-1)/ε)=', MJD
  WRITE (*,9) 'XJAEV2:max((det(U)-1)/ε)=', XJD
  WRITE (*,9) '     min(relerr(cosφ)/ε)=', MREC
  WRITE (*,9) '     max(relerr(cosφ)/ε)=', XREC
  WRITE (*,9) '     min(relerr(sinφ)/ε)=', MRES
  WRITE (*,9) '     max(relerr(sinφ)/ε)=', XRES
9 FORMAT(A,ES30.21E4)
CONTAINS
#include "orfile.F90"
#include "xrsafe.F90"
#include "qdetm1.F90"
#include "qre.F90"
END PROGRAM XTEST

PROGRAM STEST
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float, c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  REAL(KIND=c_float), PARAMETER :: HALF = 0.5_c_float
  REAL(KIND=c_float), TARGET :: A, B, C, RT1, RT2, CS1, SN1
  REAL(KIND=c_long_double), TARGET :: XA, XB, XC, XRT1, XRT2, XCS1, XSN1
  REAL(KIND=REAL128) :: D, REC, RES
  INTEGER :: U
  EXTERNAL :: SJIEV2, XJAEV2
  U = ORFILE()
  A = SRSAFE(U)
  B = SRSAFE(U)
  C = SRSAFE(U)
  CLOSE(U)
  WRITE (*,9,ADVANCE='NO') 'A(1,:):', A
  WRITE (*,9) ',', B
  WRITE (*,9,ADVANCE='NO') 'A(2,:):', B
  WRITE (*,9) ',', C
  U = 0
  CALL SJIEV2(A, B, C, RT1, RT2, CS1, SN1, U)
  IF (U .NE. 0) WRITE (*,'(A,I2)') 'SJIEV2=', U
  WRITE (*,9) ' CS1=', CS1
  WRITE (*,9) ' SN1=', SN1
  WRITE (*,9) ' RT1=', RT1
  WRITE (*,9) ' RT2=', RT2
  REC = CS1
  RES = SN1
  D = QDETM1(REC, RES)
  WRITE (*,9) 'QDET=', D
  XA = A
  XB = B
  XC = C
  CALL XJAEV2(XA, XB, XC, XRT1, XRT2, XCS1, XSN1)
  WRITE (*,9) 'XCS1=', XCS1
  WRITE (*,9) 'XSN1=', XSN1
  WRITE (*,9) 'XRT1=', XRT1
  WRITE (*,9) 'XRT2=', XRT2
  D = EPSILON(HALF) * HALF
  REC = CS1
  RES = XCS1
  REC = QRE(REC, RES, D)
  WRITE (*,9) 'QREC=', REC
  REC = SN1
  RES = XSN1
  RES = QRE(REC, RES, D)
  WRITE (*,9) 'QRES=', RES
9 FORMAT(A,ES16.9E2)
CONTAINS
#include "orfile.F90"
#include "srsafe.F90"
#include "qdetm1.F90"
#include "qre.F90"
END PROGRAM STEST

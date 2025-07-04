! IN: INFO & 1: sin => tan
!     INFO & 2: hyp
!OUT: INFO = 0: notransf
!     INFO = 1: swap only
!     INFO = 2: transf, no downscaling of A
!     INFO = 3: transf with downscaling of A
SUBROUTINE CTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, TOL, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
#include "cr.f90"
  INTERFACE
     SUBROUTINE CLJAU2(A11, A22, A21R, A21I, CS, SNR, SNI, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       REAL(KIND=REAL32), INTENT(INOUT) :: A11, A22
       REAL(KIND=REAL32), INTENT(IN) :: A21R, A21I
       REAL(KIND=REAL32), INTENT(OUT) :: CS, SNR, SNI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE CLJAU2
  END INTERFACE
  INTERFACE
     SUBROUTINE CLJAV2(A11, A22, A21R, A21I, CH, SHR, SHI, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       REAL(KIND=REAL32), INTENT(INOUT) :: A11, A22, CH
       REAL(KIND=REAL32), INTENT(IN) :: A21R, A21I
       REAL(KIND=REAL32), INTENT(OUT) :: SHR, SHI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE CLJAV2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE CRTVT(N, X, Y, CS, SNR, SNI, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N
       COMPLEX(KIND=REAL32), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=REAL32), INTENT(IN) :: CS, SNR, SNI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE CRTVT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE CRTVH(N, X, Y, CH, SHR, SHI, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N
       COMPLEX(KIND=REAL32), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=REAL32), INTENT(IN) :: CH, SHR, SHI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE CRTVH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE CRTLT(N, A, LDA, AX, P, Q, CS, SNR, SNI, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       COMPLEX(KIND=REAL32), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=REAL32), INTENT(INOUT) :: AX
       REAL(KIND=REAL32), INTENT(IN) :: CS, SNR, SNI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE CRTLT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE CRTLH(N, A, LDA, AX, P, Q, CH, SHR, SHI, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       COMPLEX(KIND=REAL32), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=REAL32), INTENT(INOUT) :: AX
       REAL(KIND=REAL32), INTENT(IN) :: CH, SHR, SHI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE CRTLH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE CRTRT(N, A, LDA, AX, P, Q, CS, SNR, SNI, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       COMPLEX(KIND=REAL32), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=REAL32), INTENT(INOUT) :: AX
       REAL(KIND=REAL32), INTENT(IN) :: CS, SNR, SNI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE CRTRT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE CRTRH(N, A, LDA, AX, P, Q, CH, SHR, SHI, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       COMPLEX(KIND=REAL32), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=REAL32), INTENT(INOUT) :: AX
       REAL(KIND=REAL32), INTENT(IN) :: CH, SHR, SHI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE CRTRH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE CSCALA(N, A, LDA, AX, AS, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA
       COMPLEX(KIND=REAL32), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=REAL32), INTENT(INOUT) :: AX
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE CSCALA
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL32
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: N, LDA, LDV, P, Q
  COMPLEX(KIND=K), INTENT(INOUT) :: A(LDA,N), V(LDV,N), TOL
  REAL(KIND=K), INTENT(INOUT) :: AX
  INTEGER, INTENT(INOUT) :: AS, INFO
  REAL(KIND=K) :: A1, A2, C, SR, SI, T
  INTEGER :: I
  T = REAL(TOL)
  IF ((INFO .LT. 0) .OR. (INFO .GT. 3)) INFO = -11
  IF (T .LT. ZERO) INFO = -10
  IF ((Q .LE. 0) .OR. (Q .GT. N)) INFO = -9
  IF ((P .LE. 0) .OR. (P .GT. N)) INFO = -8
  IF (AX .LT. ZERO) INFO = -6
  IF (LDV .LT. N) INFO = -5
  IF (LDA .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) RETURN
  I = IAND(INFO, 2)
  INFO = IAND(INFO, 1)
  A1 = REAL(A(P,P))
  A2 = REAL(A(Q,Q))
  T = (SQRT(ABS(A1)) * SQRT(ABS(A2))) * T
  TOL = ZERO
  C = ZERO
  SR = ZERO
  SI = ZERO
  IF (CR_HYPOT(REAL(A(Q,P)), AIMAG(A(Q,P))) .LT. T) THEN
     INFO = 0
  ELSE ! rotate
     T = AX
     IF (I .EQ. 0) THEN
        CALL CLJAU2(A1, A2, REAL(A(Q,P)), AIMAG(A(Q,P)), C, SR, SI, INFO)
        CALL CRTVT(N, V(1,P), V(1,Q), C, SR, SI, INFO)
        CALL CRTRT(N, A, LDA, AX, P, Q, C, SR, SI, INFO)
        CALL CRTLT(N, A, LDA, AX, P, Q, C, SR, SI, INFO)
     ELSE ! hyp
        C = CUTOFF
        CALL CLJAV2(A1, A2, REAL(A(Q,P)), AIMAG(A(Q,P)), C, SR, SI, INFO)
        CALL CRTVH(N, V(1,P), V(1,Q), C, SR, SI, INFO)
        CALL CRTRH(N, A, LDA, AX, P, Q, C, SR, SI, INFO)
        CALL CRTLH(N, A, LDA, AX, P, Q, C, SR, SI, INFO)
     END IF
     TOL = CMPLX(SR, SI, K)
     IF (INFO .LT. 0) THEN
        INFO = -4
        RETURN
     END IF
     IF (IAND(INFO, 4) .NE. 0) THEN
        I = 0
     ELSE ! transf
        I = 2
     END IF
     A(P,P) = A1
     A(Q,Q) = A2
     IF ((I .EQ. 2) .AND. (IAND(INFO, 2) .EQ. 0)) A(Q,P) = ZERO
     IF (AX .GT. T) THEN
        INFO = 1
        CALL CSCALA(N, A, LDA, AX, AS, INFO)
        IF (INFO .LT. 0) THEN
           I = -2
        ELSE IF (INFO .GT. 0) THEN
           I = 3
        ELSE ! no downscaling
           I = 2
        END IF
     END IF
     INFO = I
  END IF
END SUBROUTINE CTRANA

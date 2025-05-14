! IN: INFO & 1: sin => tan
!     INFO & 2: hyp
!OUT: INFO = 0: notransf
!     INFO = 1: swap only
!     INFO = 2: transf, no downscaling of A
!     INFO = 3: transf with downscaling of A
SUBROUTINE WTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, TOL, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  INTERFACE
     SUBROUTINE WSWPC(N, A, LDA, P, Q, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE WSWPC
  END INTERFACE
  INTERFACE
     SUBROUTINE WSWPR(N, A, LDA, P, Q, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE WSWPR
  END INTERFACE
  INTERFACE
     SUBROUTINE WLJAU2(A11, A22, A21R, A21I, CS, SNR, SNI, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(INOUT) :: A11, A22
       REAL(KIND=c_long_double), INTENT(IN) :: A21R, A21I
       REAL(KIND=c_long_double), INTENT(OUT) :: CS, SNR, SNI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WLJAU2
  END INTERFACE
  INTERFACE
     SUBROUTINE WLJAV2(A11, A22, A21R, A21I, CH, SHR, SHI, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(INOUT) :: A11, A22
       REAL(KIND=c_long_double), INTENT(IN) :: A21R, A21I
       REAL(KIND=c_long_double), INTENT(OUT) :: CH, SHR, SHI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WLJAV2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTLT(N, A, LDA, AX, P, Q, CS, SNR, SNI, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: AX
       REAL(KIND=c_long_double), INTENT(IN) :: CS, SNR, SNI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTLT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTLH(N, A, LDA, AX, P, Q, CH, SHR, SHI, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: AX
       REAL(KIND=c_long_double), INTENT(IN) :: CH, SHR, SHI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTLH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTRT(N, A, LDA, AX, P, Q, CS, SNR, SNI, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: AX
       REAL(KIND=c_long_double), INTENT(IN) :: CS, SNR, SNI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTRT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTRH(N, A, LDA, AX, P, Q, CH, SHR, SHI, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: AX
       REAL(KIND=c_long_double), INTENT(IN) :: CH, SHR, SHI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTRH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WSCALA(N, A, LDA, AX, AS, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: AX
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE WSCALA
  END INTERFACE
  INTEGER, PARAMETER :: K = c_long_double
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: N, LDA, LDV, P, Q
  COMPLEX(KIND=K), INTENT(INOUT) :: A(LDA,N), V(LDV,N), TOL
  REAL(KIND=K), INTENT(INOUT) :: AX
  INTEGER, INTENT(INOUT) :: AS, INFO
  REAL(KIND=K) :: A1, A2, VX, C, SR, SI, T
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
  IF (CR_HYPOT(REAL(A(Q,P)), AIMAG(A(Q,P))) .LT. T) THEN
     IF ((I .EQ. 0) .AND. (A1 .LT. A2)) THEN
        CALL WSWPC(N, V, LDV, P, Q, INFO)
        CALL WSWPC(N, A, LDA, P, Q, INFO)
        CALL WSWPR(N, A, LDA, P, Q, INFO)
        INFO = 1
     ELSE ! no-op
        INFO = 0
     END IF
  ELSE ! rotate
     VX = ZERO
     T = AX
     IF (I .EQ. 0) THEN
        CALL WLJAU2(A1, A2, REAL(A(Q,P)), AIMAG(A(Q,P)), C, SR, SI, INFO)
        CALL WRTRT(N, V, LDV, VX, P, Q, C, SR, SI, INFO)
        CALL WRTRT(N, A, LDA, AX, P, Q, C, SR, SI, INFO)
        CALL WRTLT(N, A, LDA, AX, P, Q, C, SR, SI, INFO)
     ELSE ! hyp
        CALL WLJAV2(A1, A2, REAL(A(Q,P)), AIMAG(A(Q,P)), C, SR, SI, INFO)
        CALL WRTRH(N, V, LDV, VX, P, Q, C, SR, SI, INFO)
        CALL WRTRH(N, A, LDA, AX, P, Q, C, SR, SI, INFO)
        CALL WRTLH(N, A, LDA, AX, P, Q, C, SR, SI, INFO)
     END IF
     TOL = CMPLX(SR, SI, K)
     IF (.NOT. (VX .LT. HUGE(VX))) THEN
        INFO = -4
        RETURN
     END IF
     IF (INFO .LT. 0) THEN
        INFO = -2
        RETURN
     END IF
     IF (IAND(INFO, 4) .NE. 0) THEN
        IF (IAND(INFO, 8) .EQ. 0) THEN
           I = 0
        ELSE ! swap
           I = 1
        END IF
     ELSE ! transf
        I = 2
     END IF
     A(P,P) = A1
     A(Q,Q) = A2
     IF (IAND(INFO, 2) .EQ. 0) THEN
        A(P,Q) = ZERO
        A(Q,P) = ZERO
     END IF
     IF (AX .GT. T) THEN
        INFO = 1
        CALL WSCALA(N, A, LDA, AX, AS, INFO)
        IF (INFO .LT. 0) THEN
           I = -7
        ELSE IF (INFO .GT. 0) THEN
           I = 3
        ELSE ! no downscaling
           I = 2
        END IF
     END IF
     INFO = I
  END IF
END SUBROUTINE WTRANA

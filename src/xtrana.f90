! IN: INFO & 1: sin => tan
!     INFO & 2: hyp
!OUT: INFO = 0: notransf
!     INFO = 1: swap only
!     INFO = 2: transf, no downscaling of A
!     INFO = 3: transf with downscaling of A
SUBROUTINE XTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, TOL, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
#ifdef USE_IEEE_INTRINSIC
#define XSQRT SQRT
#else
  INTERFACE
#ifdef __GFORTRAN__
     PURE FUNCTION XSQRT(X) BIND(C,NAME='sqrtl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
     PURE FUNCTION XSQRT(X) BIND(C,NAME='cr_sqrtq')
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X
       REAL(KIND=c_long_double) :: XSQRT
#else
       REAL(KIND=REAL128), INTENT(IN), VALUE :: X
       REAL(KIND=REAL128) :: XSQRT
#endif
     END FUNCTION XSQRT
  END INTERFACE
#endif
  INTERFACE
     SUBROUTINE XLJAU2(A11, A22, A21, CS, SN, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A11, A22
       REAL(KIND=c_long_double), INTENT(IN) :: A21
       REAL(KIND=c_long_double), INTENT(OUT) :: CS, SN
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A11, A22
       REAL(KIND=REAL128), INTENT(IN) :: A21
       REAL(KIND=REAL128), INTENT(OUT) :: CS, SN
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XLJAU2
  END INTERFACE
  INTERFACE
     SUBROUTINE XLJAV2(A11, A22, A21, CH, SH, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A11, A22, CH
       REAL(KIND=c_long_double), INTENT(IN) :: A21
       REAL(KIND=c_long_double), INTENT(OUT) :: SH
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A11, A22, CH
       REAL(KIND=REAL128), INTENT(IN) :: A21
       REAL(KIND=REAL128), INTENT(OUT) :: SH
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XLJAV2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XRTVT(N, X, Y, CS, SN, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=c_long_double), INTENT(IN) :: CS, SN
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=REAL128), INTENT(IN) :: CS, SN
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XRTVT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XRTVH(N, X, Y, CH, SH, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=c_long_double), INTENT(IN) :: CH, SH
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=REAL128), INTENT(IN) :: CH, SH
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XRTVH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XRTLT(N, A, LDA, AX, P, Q, CS, SN, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N), AX
       REAL(KIND=c_long_double), INTENT(IN) :: CS, SN
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A(LDA,N), AX
       REAL(KIND=REAL128), INTENT(IN) :: CS, SN
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XRTLT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XRTLH(N, A, LDA, AX, P, Q, CH, SH, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N), AX
       REAL(KIND=c_long_double), INTENT(IN) :: CH, SH
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A(LDA,N), AX
       REAL(KIND=REAL128), INTENT(IN) :: CH, SH
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XRTLH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XRTRT(N, A, LDA, AX, P, Q, CS, SN, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N), AX
       REAL(KIND=c_long_double), INTENT(IN) :: CS, SN
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A(LDA,N), AX
       REAL(KIND=REAL128), INTENT(IN) :: CS, SN
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XRTRT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XRTRH(N, A, LDA, AX, P, Q, CH, SH, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N), AX
       REAL(KIND=c_long_double), INTENT(IN) :: CH, SH
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A(LDA,N), AX
       REAL(KIND=REAL128), INTENT(IN) :: CH, SH
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XRTRH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XSCALA(N, A, LDA, AX, AS, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N), AX
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A(LDA,N), AX
#endif
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE XSCALA
  END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: N, LDA, LDV, P, Q
  REAL(KIND=K), INTENT(INOUT) :: A(LDA,N), V(LDV,N), AX, TOL
  INTEGER, INTENT(INOUT) :: AS, INFO
  REAL(KIND=K) :: A1, A2, C, S, T
  INTEGER :: I
  IF ((INFO .LT. 0) .OR. (INFO .GT. 3)) INFO = -11
  IF (TOL .LT. ZERO) INFO = -10
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
  A1 = A(P,P)
  A2 = A(Q,Q)
  T = (XSQRT(ABS(A1)) * XSQRT(ABS(A2))) * TOL
  TOL = ZERO
  C = ZERO
  S = ZERO
  IF (ABS(A(Q,P)) .LT. T) THEN
     INFO = 0
  ELSE ! rotate
     T = AX
     IF (I .EQ. 0) THEN
        CALL XLJAU2(A1, A2, A(Q,P), C, S, INFO)
        CALL XRTVT(N, V(1,P), V(1,Q), C, S, INFO)
        CALL XRTRT(N, A, LDA, AX, P, Q, C, S, INFO)
        CALL XRTLT(N, A, LDA, AX, P, Q, C, S, INFO)
     ELSE ! hyp
        C = CUTOFF
        CALL XLJAV2(A1, A2, A(Q,P), C, S, INFO)
        CALL XRTVH(N, V(1,P), V(1,Q), C, S, INFO)
        CALL XRTRH(N, A, LDA, AX, P, Q, C, S, INFO)
        CALL XRTLH(N, A, LDA, AX, P, Q, C, S, INFO)
     END IF
     TOL = S
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
        CALL XSCALA(N, A, LDA, AX, AS, INFO)
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
END SUBROUTINE XTRANA

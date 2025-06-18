! IN: INFO & 1: sin => tan
!     INFO & 2: hyp
!OUT: INFO = 0: notransf
!     INFO = 1: swap only
!     INFO = 2: transf, no downscaling of A
!     INFO = 3: transf with downscaling of A
SUBROUTINE WTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, TOL, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, REAL128
#endif
  IMPLICIT NONE
#ifdef __GFORTRAN__
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif
  INTERFACE
     PURE FUNCTION XSQRT(X)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: X
       REAL(KIND=c_long_double) :: XSQRT
#else
       REAL(KIND=REAL128), INTENT(IN) :: X
       REAL(KIND=REAL128) :: XSQRT
#endif
     END FUNCTION XSQRT
  END INTERFACE
  INTERFACE
     SUBROUTINE WLJAU2(A11, A22, A21R, A21I, CS, SNR, SNI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A11, A22
       REAL(KIND=c_long_double), INTENT(IN) :: A21R, A21I
       REAL(KIND=c_long_double), INTENT(OUT) :: CS, SNR, SNI
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A11, A22
       REAL(KIND=REAL128), INTENT(IN) :: A21R, A21I
       REAL(KIND=REAL128), INTENT(OUT) :: CS, SNR, SNI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WLJAU2
  END INTERFACE
  INTERFACE
     SUBROUTINE WLJAV2(A11, A22, A21R, A21I, CH, SHR, SHI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A11, A22
       REAL(KIND=c_long_double), INTENT(IN) :: A21R, A21I
       REAL(KIND=c_long_double), INTENT(OUT) :: CH, SHR, SHI
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A11, A22
       REAL(KIND=REAL128), INTENT(IN) :: A21R, A21I
       REAL(KIND=REAL128), INTENT(OUT) :: CH, SHR, SHI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WLJAV2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTVT(N, X, Y, CS, SNR, SNI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=c_long_double), INTENT(IN) :: CS, SNR, SNI
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=REAL128), INTENT(IN) :: CS, SNR, SNI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTVT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTVH(N, X, Y, CH, SHR, SHI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=c_long_double), INTENT(IN) :: CH, SHR, SHI
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=REAL128), INTENT(IN) :: CH, SHR, SHI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTVH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTLT(N, A, LDA, AX, P, Q, CS, SNR, SNI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: AX
       REAL(KIND=c_long_double), INTENT(IN) :: CS, SNR, SNI
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=REAL128), INTENT(INOUT) :: AX
       REAL(KIND=REAL128), INTENT(IN) :: CS, SNR, SNI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTLT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTLH(N, A, LDA, AX, P, Q, CH, SHR, SHI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: AX
       REAL(KIND=c_long_double), INTENT(IN) :: CH, SHR, SHI
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=REAL128), INTENT(INOUT) :: AX
       REAL(KIND=REAL128), INTENT(IN) :: CH, SHR, SHI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTLH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTRT(N, A, LDA, AX, P, Q, CS, SNR, SNI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: AX
       REAL(KIND=c_long_double), INTENT(IN) :: CS, SNR, SNI
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=REAL128), INTENT(INOUT) :: AX
       REAL(KIND=REAL128), INTENT(IN) :: CS, SNR, SNI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTRT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTRH(N, A, LDA, AX, P, Q, CH, SHR, SHI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, P, Q
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: AX
       REAL(KIND=c_long_double), INTENT(IN) :: CH, SHR, SHI
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=REAL128), INTENT(INOUT) :: AX
       REAL(KIND=REAL128), INTENT(IN) :: CH, SHR, SHI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTRH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WSCALA(N, A, LDA, AX, AS, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: AX
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=REAL128), INTENT(INOUT) :: AX
#endif
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE WSCALA
  END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
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
  T = (XSQRT(ABS(A1)) * XSQRT(ABS(A2))) * T
  TOL = ZERO
#ifdef CARITH_PVN
  WRITE (ERROR_UNIT,9) P, SCALE(REAL(A(P,P)), -AS), SCALE(REAL(A(Q,P)), -AS), SCALE(REAL(A(4,2)), -AS)
  WRITE (ERROR_UNIT,9) Q, SCALE(AIMAG(A(Q,P)), -AS), SCALE(REAL(A(Q,Q)), -AS), SCALE(AIMAG(A(4,2)), -AS)
#endif
  C = ZERO
  SR = ZERO
  SI = ZERO
  IF (CR_HYPOT(REAL(A(Q,P)), AIMAG(A(Q,P))) .LT. T) THEN
     INFO = 0
  ELSE ! rotate
     T = AX
     IF (I .EQ. 0) THEN
        CALL WLJAU2(A1, A2, REAL(A(Q,P)), AIMAG(A(Q,P)), C, SR, SI, INFO)
        CALL WRTVT(N, V(1,P), V(1,Q), C, SR, SI, INFO)
        CALL WRTRT(N, A, LDA, AX, P, Q, C, SR, SI, INFO)
        CALL WRTLT(N, A, LDA, AX, P, Q, C, SR, SI, INFO)
     ELSE ! hyp
        CALL WLJAV2(A1, A2, REAL(A(Q,P)), AIMAG(A(Q,P)), C, SR, SI, INFO)
        CALL WRTVH(N, V(1,P), V(1,Q), C, SR, SI, INFO)
        CALL WRTRH(N, A, LDA, AX, P, Q, C, SR, SI, INFO)
        CALL WRTLH(N, A, LDA, AX, P, Q, C, SR, SI, INFO)
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
     A(P,P) = CMPLX(A1, ZERO, K)
     A(Q,Q) = CMPLX(A2, ZERO, K)
     IF ((I .EQ. 2) .AND. (IAND(INFO, 2) .EQ. 0)) A(Q,P) = CMPLX(ZERO, ZERO, K)
     IF (AX .GT. T) THEN
        INFO = 1
        CALL WSCALA(N, A, LDA, AX, AS, INFO)
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
#ifdef CARITH_PVN
#ifdef __GFORTRAN__
9 FORMAT(I1,3(ES31.21E4))
#else
9 FORMAT(I1,3(ES46.36E4))
#endif
#endif
END SUBROUTINE WTRANA

! IN: INFO & 1: hyp
!     INFO & 2: SLOW
!OUT: INFO = 0: notransf due to tol
!     INFO = 1: notransf due to identity transf
!     INFO = 2: transf
!     INFO = 3: transf, big th
!     ... OR 4: downscaling of G and SV
SUBROUTINE WTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, IX, WRK, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  IMPLICIT NONE
#include "cr.f90"
  INTERFACE
     PURE FUNCTION WNRMF(M, X)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(IN) :: X(M)
       REAL(KIND=c_long_double) :: WNRMF
#else
       COMPLEX(KIND=REAL128), INTENT(IN) :: X(M)
       REAL(KIND=REAL128) :: WNRMF
#endif
     END FUNCTION WNRMF
  END INTERFACE
  INTERFACE
     FUNCTION WSDP(M, X, Y, MX, MY, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       INTEGER, INTENT(INOUT) :: INFO
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(IN) :: X(M), Y(M)
       REAL(KIND=c_long_double), INTENT(IN) :: MX, MY
       COMPLEX(KIND=c_long_double) :: WSDP
#else
       COMPLEX(KIND=REAL128), INTENT(IN) :: X(M), Y(M)
       REAL(KIND=REAL128), INTENT(IN) :: MX, MY
       COMPLEX(KIND=REAL128) :: WSDP
#endif
     END FUNCTION WSDP
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WGRAM(PNF, QNF, QPS, APP, AQQ, AQPR, AQPI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: PNF, QNF
       COMPLEX(KIND=c_long_double), INTENT(IN) :: QPS
       REAL(KIND=c_long_double), INTENT(OUT) :: APP, AQQ, AQPR, AQPI
#else
       REAL(KIND=REAL128), INTENT(IN) :: PNF, QNF
       COMPLEX(KIND=REAL128), INTENT(IN) :: QPS
       REAL(KIND=REAL128), INTENT(OUT) :: APP, AQQ, AQPR, AQPI
#endif
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE WGRAM
  END INTERFACE
  INTERFACE
     SUBROUTINE WLJTU2(A11, A22, A21R, A21I, CS, TNR, TNI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A11, A22, A21R, A21I
       REAL(KIND=c_long_double), INTENT(OUT) :: CS, TNR, TNI
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A11, A22, A21R, A21I
       REAL(KIND=REAL128), INTENT(OUT) :: CS, TNR, TNI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WLJTU2
  END INTERFACE
  INTERFACE
     SUBROUTINE WLJTV2(A11, A22, A21R, A21I, CH, THR, THI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A11, A22, A21R, A21I, CH
       REAL(KIND=c_long_double), INTENT(OUT) :: THR, THI
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A11, A22, A21R, A21I, CH
       REAL(KIND=REAL128), INTENT(OUT) :: THR, THI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WLJTV2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTT(M, X, Y, CS, TNR, TNI, GX, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: X(M), Y(M)
       REAL(KIND=c_long_double), INTENT(IN) :: CS, TNR, TNI
       REAL(KIND=c_long_double), INTENT(INOUT) :: GX
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: X(M), Y(M)
       REAL(KIND=REAL128), INTENT(IN) :: CS, TNR, TNI
       REAL(KIND=REAL128), INTENT(INOUT) :: GX
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTH(M, X, Y, CH, THR, THI, GX, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: X(M), Y(M)
       REAL(KIND=c_long_double), INTENT(IN) :: CH, THR, THI
       REAL(KIND=c_long_double), INTENT(INOUT) :: GX
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: X(M), Y(M)
       REAL(KIND=REAL128), INTENT(IN) :: CH, THR, THI
       REAL(KIND=REAL128), INTENT(INOUT) :: GX
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTH
  END INTERFACE
  INTERFACE
#ifdef _OPENMP
     SUBROUTINE WSCALG(M, N, G, LDG, GX, GS, INFO)
#else
     PURE SUBROUTINE WSCALG(M, N, G, LDG, GX, GS, INFO)
#endif
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: GX
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=REAL128), INTENT(INOUT) :: GX
#endif
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE WSCALG
  END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q, IX(N)
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N), V(LDV,N), TOL
  COMPLEX(KIND=K), INTENT(OUT) :: WRK(M,N)
  REAL(KIND=K), INTENT(INOUT) :: SV(N), GX
  INTEGER, INTENT(INOUT) :: GS, INFO
  COMPLEX(KIND=K) :: QPS
  REAL(KIND=K) :: APP, AQQ, AQPR, AQPI, C, S, T, TR, TI
  INTEGER :: I, J, L, O
#ifndef NDEBUG
  IF ((INFO .LT. 0) .OR. (INFO .GT. 3)) INFO = -15
  IF (REAL(TOL) .LT. ZERO) INFO = -12
  IF ((Q .LE. 0) .OR. (Q .GT. N)) INFO = -11
  IF ((P .LE. 0) .OR. (P .GT. N)) INFO = -10
  IF (GX .LT. ZERO) INFO = -8
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  L = IAND(INFO, 1)
  O = IAND(INFO, 2)
  IF (O .EQ. 0) THEN
     I = 1
  ELSE ! SLOW
     I = 0
  END IF
  QPS = WSDP(M, G(1,IX(Q)), G(1,IX(P)), SV(Q), SV(P), I)
#ifndef NDEBUG
  IF (I .LT. 0) THEN
     INFO = -3
     RETURN
  END IF
#endif
  S = CR_HYPOT(REAL(QPS), AIMAG(QPS))
#ifndef NDEBUG
  IF (.NOT. (S .LE. HUGE(S))) THEN
     INFO = -14
     RETURN
  END IF
#endif
  C = ONE
  T = AIMAG(TOL)
  TR = ZERO
  TI = ZERO
  IF (S .LT. REAL(TOL)) THEN
     TOL = ZERO
     INFO = 0
     GOTO 9
  END IF
  J = 0
  CALL WGRAM(SV(P), SV(Q), QPS, APP, AQQ, AQPR, AQPI, J)
#ifndef NDEBUG
  IF (J .LE. -HUGE(J)) THEN
     INFO = -7
     RETURN
  END IF
#endif
  I = 0
  T = GX
  IF (L .EQ. 0) THEN
     CALL WLJTU2(APP, AQQ, AQPR, AQPI, C, TR, TI, I)
     IF (I .GT. 0) THEN
        J = 0
        CALL WRTT(N, V(1,IX(P)), V(1,IX(Q)), C, TR, TI, T, J)
#ifndef NDEBUG
        IF (J .LT. 0) THEN
           INFO = -5
           RETURN
        END IF
#endif
        J = 1
        CALL WRTT(M, G(1,IX(P)), G(1,IX(Q)), C, TR, TI, T, J)
#ifndef NDEBUG
        IF (J .LT. 0) THEN
           INFO = -13
           RETURN
        END IF
#endif
     END IF
  ELSE ! hyp
     C = CUTOFF
     CALL WLJTV2(APP, AQQ, AQPR, AQPI, C, TR, TI, I)
     IF (I .GT. 0) THEN
        J = 0
        CALL WRTH(N, V(1,IX(P)), V(1,IX(Q)), C, TR, TI, T, J)
#ifndef NDEBUG
        IF (J .LT. 0) THEN
           INFO = -5
           RETURN
        END IF
#endif
        J = 1
        CALL WRTH(M, G(1,IX(P)), G(1,IX(Q)), C, TR, TI, T, J)
#ifndef NDEBUG
        IF (J .LT. 0) THEN
           INFO = -13
           RETURN
        END IF
#endif
     END IF
  END IF
  TOL = CMPLX(AQPR, AQPI, K)
  IF (I .EQ. 0) THEN
     INFO = 1
  ELSE IF (I .LT. 0) THEN
     INFO = -8
  ELSE ! I > 0
     IF (O .EQ. 0) THEN
        ! S = ABS(A21 / (SV(P) * SV(Q)))
        ! norm update, trig:
        ! SQRT(SV(P) + TG * (S * SV(Q))) * SQRT(SV(P))
        ! SQRT(SV(Q) - TG * (S * SV(P))) * SQRT(SV(Q))
        ! norm update, hyp:
        ! SQRT(SV(P) + TH * (S * SV(Q))) * SQRT(SV(P))
        ! SQRT(SV(Q) + TH * (S * SV(P))) * SQRT(SV(Q))
        APP = S * SV(Q)
        AQQ = S * SV(P)
        IF (L .EQ. 0) THEN
           AQPI = -AQPR
        ELSE ! hyp
           AQPI =  AQPR
        END IF
        APP = CR_SQRT(XFMA(AQPR, APP, SV(P)))
        AQQ = CR_SQRT(XFMA(AQPI, AQQ, SV(Q)))
        SV(P) = APP * CR_SQRT(SV(P))
        SV(Q) = AQQ * CR_SQRT(SV(Q))
     ELSE ! SLOW
        SV(P) = WNRMF(M, G(1,IX(P)))
        SV(Q) = WNRMF(M, G(1,IX(Q)))
     END IF
     INFO = I + 1
     IF (T .GT. GX) THEN
        GX = T
        I = -INFO
        CALL WSCALG(M, N, G, LDG, GX, GS, I)
        IF (I .GT. 0) THEN
           I = -I
           DO J = 1, N
              SV(J) = SCALE(SV(J), I)
           END DO
           INFO = IOR(INFO, 4)
#ifndef NDEBUG
        ELSE IF (I .LT. 0) THEN
           INFO = -9
#endif
        END IF
     END IF
  END IF
9 WRK(P,Q) = CMPLX(C, S, K)
  WRK(Q,P) = CMPLX(TR, TI, K)
END SUBROUTINE WTRNSF

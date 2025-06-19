! IN: INFO & 1: hyp
!     INFO & 2: SLOW
!OUT: INFO = 0: notransf due to tol
!     INFO = 1: notransf due to identity transf
!     INFO = 2: transf
!     INFO = 3: transf, big th
!     ... OR 4: downscaling of G and SV
! ~Rutishauser:
! X1 = X
! C1 = 1
! Z1 = (/ 0, ..., 0 /)
! Xj = X1 * C2 * *** * Cj + Zj
!    = X1 * CC + Zj
SUBROUTINE WTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, IX, WRK, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION WFMA(A, B, C)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(IN) :: A, B, C
       COMPLEX(KIND=c_long_double) :: WFMA
#else
       COMPLEX(KIND=REAL128), INTENT(IN) :: A, B, C
       COMPLEX(KIND=REAL128) :: WFMA
#endif
     END FUNCTION WFMA
  END INTERFACE
#ifdef USE_IEEE_INTRINSIC
#define XFMA IEEE_FMA
#else
  INTERFACE
     PURE FUNCTION XFMA(A, B, C)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: A, B, C
       REAL(KIND=c_long_double) :: XFMA
#else
       REAL(KIND=REAL128), INTENT(IN) :: A, B, C
       REAL(KIND=REAL128) :: XFMA
#endif
     END FUNCTION XFMA
  END INTERFACE
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
       REAL(KIND=c_long_double), INTENT(INOUT) :: A11, A22, A21R, A21I
       REAL(KIND=c_long_double), INTENT(OUT) :: CH, THR, THI
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A11, A22, A21R, A21I
       REAL(KIND=REAL128), INTENT(OUT) :: CH, THR, THI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WLJTV2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WSCALG(M, N, G, LDG, GX, GS, INFO)
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
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N), V(LDV,N), TOL, WRK(M,N+1)
  REAL(KIND=K), INTENT(INOUT) :: SV(N), GX
  INTEGER, INTENT(INOUT) :: GS, INFO
  COMPLEX(KIND=K) :: QPS, XX, YY, CQN, ZZ
  REAL(KIND=K) :: APP, AQQ, AQPR, AQPI, C, S, T, TR, TI, CC
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
  J = IX(P)
  CC = REAL(WRK(P,N+1))
  DO I = 1, M
     WRK(I,N) = CMPLX(XFMA(REAL(G(I,J)), CC, REAL(WRK(I,P))), XFMA(AIMAG(G(I,J)), CC, AIMAG(WRK(I,P))), K)
  END DO
  IF (IAND(INFO, 2) .EQ. 0) THEN
     I = 1
  ELSE ! SLOW
     I = 0
  END IF
  QPS = WSDP(M, G(1,IX(Q)), WRK(1,N), SV(Q), SV(P), I)
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
  T = GX
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
  IF (IAND(INFO, 1) .EQ. 0) THEN
     CALL WLJTU2(APP, AQQ, AQPR, AQPI, C, TR, TI, I)
     IF (I .GT. 0) THEN
        QPS = CMPLX( TR, TI, K)
        CQN = CMPLX(-TR, TI, K)
        O = IX(Q)
        DO L = 1, M
           XX = WRK(L,P) ! ZZ
           YY = G(L,O)
           ! (YY * QPS + XX) * C
           XX = WFMA(YY, QPS, XX)
           XX = CMPLX(REAL(XX) * C, AIMAG(XX) * C, K)
           WRK(L,P) = XX
           T = MAX(T, MAX(ABS(REAL(XX)), ABS(AIMAG(XX))))
           XX = WRK(L,N) ! XX
           ! (YY - XX * CONJG(QPS)) * C
           YY = WFMA(XX, CQN, YY)
           YY = CMPLX(REAL(YY) * C, AIMAG(YY) * C, K)
           G(L,O) = YY
           T = MAX(T, MAX(ABS(REAL(YY)), ABS(AIMAG(YY))))
        END DO
        J = IX(P)
        DO L = 1, N
           XX = V(L,J)
           YY = V(L,O)
           ! XX = (YY * QPS + XX) * C
           ZZ = ZFMA(YY, QPS, XX)
           V(L,J) = CMPLX(REAL(ZZ) * C, AIMAG(ZZ) * C, K)
           ! YY = (YY - XX * CONJG(QPS)) * C
           ZZ = ZFMA(XX, CQN, YY)
           V(L,O) = CMPLX(REAL(ZZ) * C, AIMAG(ZZ) * C, K)
        END DO
     END IF
  ELSE ! hyp
     CALL WLJTV2(APP, AQQ, AQPR, AQPI, C, TR, TI, I)
     IF (I .GT. 0) THEN
        QPS = CMPLX(TR,  TI, K)
        CQN = CMPLX(TR, -TI, K)
        O = IX(Q)
        DO L = 1, M
           XX = WRK(L,P) ! ZZ
           YY = G(L,O)
           ! XX = (YY * QPS + XX) * C
           XX = WFMA(YY, QPS, XX)
           XX = CMPLX(REAL(XX) * C, AIMAG(XX) * C, K)
           WRK(L,P) = XX
           T = MAX(T, MAX(ABS(REAL(XX)), ABS(AIMAG(XX))))
           XX = WRK(L,N) ! XX
           ! YY = (XX * CONJG(QPS) + YY) * C
           YY = WFMA(XX, CQN, YY)
           YY = CMPLX(REAL(YY) * C, AIMAG(YY) * C, K)
           G(L,O) = YY
           T = MAX(T, MAX(ABS(REAL(YY)), ABS(AIMAG(YY))))
        END DO
        J = IX(P)
        DO L = 1, N
           XX = V(L,J)
           YY = V(L,O)
           ! XX = (YY * QPS + XX) * C
           ZZ = ZFMA(YY, QPS, XX)
           V(L,J) = CMPLX(REAL(ZZ) * C, AIMAG(ZZ) * C, K)
           ! YY = (XX * CONJG(QPS) + YY) * C
           ZZ = ZFMA(XX, CQN, YY)
           V(L,O) = CMPLX(REAL(ZZ) * C, AIMAG(ZZ) * C, K)
        END DO
     END IF
  END IF
  TOL = CMPLX(AQPR, AQPI, K)
  IF (I .EQ. 0) THEN
     INFO = 1
  ELSE IF (I .LT. 0) THEN
     INFO = -5
     RETURN
  ELSE ! I > 0
     CC = CC * C
     ! S = ABS(A21 / (SV(P) * SV(Q)))
     ! norm update, trig:
     ! SQRT(SV(P) + TG * (S * SV(Q))) * SQRT(SV(P))
     ! SQRT(SV(Q) - TG * (S * SV(P))) * SQRT(SV(Q))
     ! norm update, hyp:
     ! SQRT(SV(P) + TH * (S * SV(Q))) * SQRT(SV(P))
     ! SQRT(SV(Q) + TH * (S * SV(P))) * SQRT(SV(Q))
     APP = S * SV(Q)
     AQQ = S * SV(P)
     IF (IAND(INFO, 1) .EQ. 0) THEN
        AQPI = -AQPR
     ELSE ! hyp
        AQPI =  AQPR
     END IF
     APP = XSQRT(XFMA(AQPR, APP, SV(P)))
     AQQ = XSQRT(XFMA(AQPI, AQQ, SV(Q)))
     SV(P) = APP * XSQRT(SV(P))
     SV(Q) = AQQ * XSQRT(SV(Q))
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
           DO J = 1, N-1
              DO L = 1, M
                 WRK(L,J) = CMPLX(SCALE(REAL(WRK(L,J)), I), SCALE(AIMAG(WRK(L,J)), I), K)
              END DO
           END DO
           INFO = IOR(INFO, 4)
#ifndef NDEBUG
        ELSE IF (I .LT. 0) THEN
           INFO = -9
#endif
        END IF
     END IF
  END IF
9 IF (Q .EQ. N) THEN
     J = IX(P)
     DO I = 1, M
        G(I,J) = CMPLX(XFMA(REAL(G(I,J)), CC, REAL(WRK(I,P))), XFMA(AIMAG(G(I,J)), CC, AIMAG(WRK(I,P))), K)
        T = MAX(T, MAX(ABS(REAL(G(I,J))), ABS(AIMAG(G(I,J)))))
     END DO
     IF (T .GT. GX) THEN
        GX = T
        I = -2
        CALL WSCALG(M, N, G, LDG, GX, GS, I)
        IF (I .GT. 0) THEN
           I = -I
           DO J = 1, N
              SV(J) = SCALE(SV(J), I)
           END DO
           DO J = 1, N-1
              DO L = 1, M
                 WRK(L,J) = CMPLX(SCALE(REAL(WRK(L,J)), I), SCALE(AIMAG(WRK(L,J)), I), K)
              END DO
           END DO
           INFO = IOR(INFO, 4)
#ifndef NDEBUG
        ELSE IF (I .LT. 0) THEN
           INFO = -9
#endif
        END IF
     END IF
  END IF
  WRK(P,N+1) = CMPLX(CC, AIMAG(WRK(P,N+1)) + ONE, K)
END SUBROUTINE WTRUTI

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
SUBROUTINE ZTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, IX, WRK, RWRK, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  IMPLICIT NONE
#include "cr.f90"
  INTERFACE
     FUNCTION ZSDP(M, X, Y, MX, MY, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       INTEGER, INTENT(INOUT) :: INFO
       COMPLEX(KIND=REAL64), INTENT(IN) :: X(M), Y(M)
       REAL(KIND=REAL64), INTENT(IN) :: MX, MY
       COMPLEX(KIND=REAL64) :: ZSDP
     END FUNCTION ZSDP
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE ZGRAM(PNF, QNF, QPS, APP, AQQ, AQPR, AQPI, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       REAL(KIND=REAL64), INTENT(IN) :: PNF, QNF
       COMPLEX(KIND=REAL64), INTENT(IN) :: QPS
       REAL(KIND=REAL64), INTENT(OUT) :: APP, AQQ, AQPR, AQPI
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE ZGRAM
  END INTERFACE
  INTERFACE
     SUBROUTINE ZLJTU2(A11, A22, A21R, A21I, CS, TNR, TNI, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       REAL(KIND=REAL64), INTENT(INOUT) :: A11, A22, A21R, A21I
       REAL(KIND=REAL64), INTENT(OUT) :: CS, TNR, TNI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE ZLJTU2
  END INTERFACE
  INTERFACE
     SUBROUTINE ZLJTV2(A11, A22, A21R, A21I, CH, THR, THI, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       REAL(KIND=REAL64), INTENT(INOUT) :: A11, A22, A21R, A21I, CH
       REAL(KIND=REAL64), INTENT(OUT) :: THR, THI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE ZLJTV2
  END INTERFACE
  INTERFACE
#ifdef _OPENMP
     SUBROUTINE ZSCALG(M, N, G, LDG, GX, GS, INFO)
#else
     PURE SUBROUTINE ZSCALG(M, N, G, LDG, GX, GS, INFO)
#endif
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
       COMPLEX(KIND=REAL64), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=REAL64), INTENT(INOUT) :: GX
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE ZSCALG
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q, IX(N)
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N), V(LDV,N), TOL, WRK(M,N)
  REAL(KIND=K), INTENT(INOUT) :: SV(N), GX, RWRK(N)
  INTEGER, INTENT(INOUT) :: GS, INFO
  COMPLEX(KIND=K) :: QPS, XX, YY, CQN, ZZ
  REAL(KIND=K) :: APP, AQQ, AQPR, AQPI, C, S, T, TR, TI, CC
  INTEGER :: I, J, L, O
#ifndef NDEBUG
  IF ((INFO .LT. 0) .OR. (INFO .GT. 3)) INFO = -16
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
  CC = RWRK(P)
  DO I = 1, M
     WRK(I,N) = CMPLX(DFMA(REAL(G(I,J)), CC, REAL(WRK(I,P))), DFMA(AIMAG(G(I,J)), CC, AIMAG(WRK(I,P))), K)
  END DO
  IF (IAND(INFO, 2) .EQ. 0) THEN
     I = 1
  ELSE ! SLOW
     I = 0
  END IF
  QPS = ZSDP(M, G(1,IX(Q)), WRK(1,N), SV(Q), SV(P), I)
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
  CALL ZGRAM(SV(P), SV(Q), QPS, APP, AQQ, AQPR, AQPI, J)
#ifndef NDEBUG
  IF (J .LE. -HUGE(J)) THEN
     INFO = -7
     RETURN
  END IF
#endif
  I = 0
  IF (IAND(INFO, 1) .EQ. 0) THEN
     CALL ZLJTU2(APP, AQQ, AQPR, AQPI, C, TR, TI, I)
     IF (I .GT. 0) THEN
        QPS = CMPLX( TR, TI, K)
        CQN = CMPLX(-TR, TI, K)
        O = IX(Q)
        DO L = 1, M
           XX = WRK(L,P) ! ZZ
           YY = G(L,O)
           ! (YY * QPS + XX) * C
           XX = ZFMA(YY, QPS, XX)
           XX = CMPLX(REAL(XX) * C, AIMAG(XX) * C, K)
           WRK(L,P) = XX
           T = MAX(T, MAX(ABS(REAL(XX)), ABS(AIMAG(XX))))
           XX = WRK(L,N) ! XX
           ! (YY - XX * CONJG(QPS)) * C
           YY = ZFMA(XX, CQN, YY)
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
     C = CUTOFF
     CALL ZLJTV2(APP, AQQ, AQPR, AQPI, C, TR, TI, I)
     IF (I .GT. 0) THEN
        QPS = CMPLX(TR,  TI, K)
        CQN = CMPLX(TR, -TI, K)
        O = IX(Q)
        DO L = 1, M
           XX = WRK(L,P) ! ZZ
           YY = G(L,O)
           ! XX = (YY * QPS + XX) * C
           XX = ZFMA(YY, QPS, XX)
           XX = CMPLX(REAL(XX) * C, AIMAG(XX) * C, K)
           WRK(L,P) = XX
           T = MAX(T, MAX(ABS(REAL(XX)), ABS(AIMAG(XX))))
           XX = WRK(L,N) ! XX
           ! YY = (XX * CONJG(QPS) + YY) * C
           YY = ZFMA(XX, CQN, YY)
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
     APP = SQRT(DFMA(AQPR, APP, SV(P)))
     AQQ = SQRT(DFMA(AQPI, AQQ, SV(Q)))
     SV(P) = APP * SQRT(SV(P))
     SV(Q) = AQQ * SQRT(SV(Q))
     INFO = I + 1
     IF (T .GT. GX) THEN
        GX = T
        I = -INFO
        CALL ZSCALG(M, N, G, LDG, GX, GS, I)
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
        G(I,J) = CMPLX(DFMA(REAL(G(I,J)), CC, REAL(WRK(I,P))), DFMA(AIMAG(G(I,J)), CC, AIMAG(WRK(I,P))), K)
        T = MAX(T, MAX(ABS(REAL(G(I,J))), ABS(AIMAG(G(I,J)))))
     END DO
     IF (T .GT. GX) THEN
        GX = T
        I = -2
        CALL ZSCALG(M, N, G, LDG, GX, GS, I)
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
  RWRK(P) = CC
END SUBROUTINE ZTRUTI

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
SUBROUTINE ZTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, IX, WRK, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION DFMA(A, B, C)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       REAL(KIND=REAL64), INTENT(IN) :: A, B, C
       REAL(KIND=REAL64) :: DFMA
     END FUNCTION DFMA
  END INTERFACE
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       IMPLICIT NONE
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  INTERFACE
     PURE FUNCTION ZNRMF(M, X)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       COMPLEX(KIND=REAL64), INTENT(IN) :: X(M)
       REAL(KIND=REAL64) :: ZNRMF
     END FUNCTION ZNRMF
  END INTERFACE
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
       REAL(KIND=REAL64), INTENT(INOUT) :: A11, A22, A21R, A21I
       REAL(KIND=REAL64), INTENT(OUT) :: CH, THR, THI
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE ZLJTV2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE ZRTT(M, X, Y, CS, TNR, TNI, GX, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       COMPLEX(KIND=REAL64), INTENT(INOUT) :: X(M), Y(M)
       REAL(KIND=REAL64), INTENT(IN) :: CS, TNR, TNI
       REAL(KIND=REAL64), INTENT(INOUT) :: GX
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE ZRTT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE ZRTH(M, X, Y, CH, THR, THI, GX, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       COMPLEX(KIND=REAL64), INTENT(INOUT) :: X(M), Y(M)
       REAL(KIND=REAL64), INTENT(IN) :: CH, THR, THI
       REAL(KIND=REAL64), INTENT(INOUT) :: GX
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE ZRTH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE ZSCALG(M, N, G, LDG, GX, GS, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
       COMPLEX(KIND=REAL64), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=REAL64), INTENT(INOUT) :: GX
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE ZSCALG
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q, IX(N)
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N), V(LDV,N), TOL, WRK(M,N)
  REAL(KIND=K), INTENT(INOUT) :: SV(N), GX
  INTEGER, INTENT(INOUT) :: GS, INFO
  COMPLEX(KIND=K) :: QPS, XX, YY
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
  IF ((Q - P) .EQ. 1) THEN
     CC = ONE
     DO I = 1, M
        WRK(I,P) = ZERO
        WRK(I,N) = G(I,J)
     END DO
  ELSE ! Q - P > 1
     CC = REAL(WRK(P,N))
     DO I = 1, M
        !DIR$ FMA
        WRK(I,N) = CMPLX(REAL(G(I,J)) * CC + REAL(WRK(I,P)), AIMAG(G(I,J)) * CC + AIMAG(WRK(I,P)), K)
     END DO
  END IF
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
        QPS = CMPLX(TR, TI, K)
        O = IX(Q)
        DO L = 1, M
           XX = WRK(L,P) ! ZZ
           YY = G(L,O)
           ! (YY * QPS + XX) * CS
           !DIR$ FMA
           WRK(L,P) = CMPLX((REAL(YY) * REAL(QPS) + (REAL(XX) - AIMAG(YY) * AIMAG(QPS))) * C,&
                (REAL(YY) * AIMAG(QPS) + (AIMAG(XX) + AIMAG(YY) * REAL(QPS))) * C, K)
           XX = WRK(L,N) ! XX
           ! (YY - XX * CONJG(QPS)) * C
           !DIR$ FMA
           G(L,O) = CMPLX(((REAL(YY) - AIMAG(XX) * AIMAG(QPS)) - REAL(XX) * REAL(QPS)) * C,&
                (REAL(XX) * AIMAG(QPS) + (AIMAG(YY) - AIMAG(XX) * REAL(QPS))) * C, K)
           T = MAX(T, REAL(G(L,O)), AIMAG(G(L,O)))
        END DO
        J = IX(P)
        DO L = 1, N
           XX = V(L,J)
           YY = V(L,O)
           ! XX = (YY * QPS + XX) * C
           !DIR$ FMA
           V(L,J) = CMPLX((REAL(YY) * REAL(QPS) + (REAL(XX) - AIMAG(YY) * AIMAG(QPS))) * C,&
             (REAL(YY) * AIMAG(QPS) + (AIMAG(XX) + AIMAG(YY) * REAL(QPS))) * C, K)
           ! YY = (YY - XX * CONJG(QPS)) * C
           !DIR$ FMA
           V(L,O) = CMPLX(((REAL(YY) - AIMAG(XX) * AIMAG(QPS)) - REAL(XX) * REAL(QPS)) * C,&
                (REAL(XX) * AIMAG(QPS) + (AIMAG(YY) - AIMAG(XX) * REAL(QPS))) * C, K)
        END DO
     END IF
  ELSE ! hyp
     CALL ZLJTV2(APP, AQQ, AQPR, AQPI, C, TR, TI, I)
     IF (I .GT. 0) THEN
        QPS = CMPLX(TR, TI, K)
        O = IX(Q)
        DO L = 1, M
           XX = WRK(L,P) ! ZZ
           YY = G(L,O)
           ! XX = (YY * QPS + XX) * C
           !DIR$ FMA
           WRK(L,P) = CMPLX((REAL(YY) * REAL(QPS) + (REAL(XX) - AIMAG(YY) * AIMAG(QPS))) * C,&
                (REAL(YY) * AIMAG(QPS) + (AIMAG(XX) + AIMAG(YY) * REAL(QPS))) * C, K)
           XX = WRK(L,N) ! XX
           ! YY = (XX * CONJG(QPS) + YY) * C
           !DIR$ FMA
           G(L,O) = CMPLX((REAL(XX) * REAL(QPS) + (REAL(YY) + AIMAG(XX) * AIMAG(QPS))) * C,&
                ((AIMAG(YY) + AIMAG(XX) * REAL(QPS)) - REAL(XX) * AIMAG(QPS)) * C, K)
           T = MAX(T, REAL(G(L,O)), AIMAG(G(L,O)))
        END DO
        DO L = 1, N
           XX = V(L,J)
           YY = V(L,O)
           ! XX = (YY * QPS + XX) * C
           !DIR$ FMA
           V(L,J) = CMPLX((REAL(YY) * REAL(QPS) + (REAL(XX) - AIMAG(YY) * AIMAG(QPS))) * C,&
                (REAL(YY) * AIMAG(QPS) + (AIMAG(XX) + AIMAG(YY) * REAL(QPS))) * C, K)
           ! YY = (XX * CONJG(QPS) + YY) * C
           !DIR$ FMA
           V(L,O) = CMPLX((REAL(XX) * REAL(QPS) + (REAL(YY) + AIMAG(XX) * AIMAG(QPS))) * C,&
                ((AIMAG(YY) + AIMAG(XX) * REAL(QPS)) - REAL(XX) * AIMAG(QPS)) * C, K)
        END DO
     END IF
  END IF
  TOL = CMPLX(AQPR, AQPI, K)
  IF (I .EQ. 0) THEN
     INFO = 1
  ELSE IF (I .LT. 0) THEN
     INFO = -8
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
        !DIR$ FMA
        G(I,J) = CMPLX(REAL(G(I,J)) * CC + REAL(WRK(I,P)), AIMAG(G(I,J)) * CC + AIMAG(WRK(I,P)), K)
        T = MAX(T, ABS(REAL(G(I,J))), ABS(AIMAG(G(I,J))))
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
           INFO = IOR(INFO, 4)
#ifndef NDEBUG
        ELSE IF (I .LT. 0) THEN
           INFO = -9
#endif
        END IF
     END IF
  END IF
  WRK(P,N) = CC
END SUBROUTINE ZTRUTI

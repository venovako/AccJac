! IN: INFO & 1: hyp
!     INFO & 2: SLOW
!OUT: INFO = 0: notransf due to tol
!     INFO = 1: notransf due to identity transf
!     INFO = 2: transf
!     INFO = 3: transf, big th
!     ... OR 4: downscaling of G and SV
SUBROUTINE DTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, IX, WRK, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  IMPLICIT NONE
#ifdef USE_IEEE_INTRINSIC
#define DFMA IEEE_FMA
#else
  INTERFACE
     PURE FUNCTION DFMA(A, B, C) BIND(C,NAME='fma')
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       REAL(KIND=REAL64), INTENT(IN), VALUE :: A, B, C
       REAL(KIND=REAL64) :: DFMA
     END FUNCTION DFMA
  END INTERFACE
#endif
  INTERFACE
     PURE FUNCTION DNRMF(M, X)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       REAL(KIND=REAL64), INTENT(IN) :: X(M)
       REAL(KIND=REAL64) :: DNRMF
     END FUNCTION DNRMF
  END INTERFACE
  INTERFACE
     FUNCTION DSDP(M, X, Y, MX, MY, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       INTEGER, INTENT(INOUT) :: INFO
       REAL(KIND=REAL64), INTENT(IN) :: X(M), Y(M), MX, MY
       REAL(KIND=REAL64) :: DSDP
     END FUNCTION DSDP
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE DGRAM(PNF, QNF, QPS, APP, AQQ, AQP, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       REAL(KIND=REAL64), INTENT(IN) :: PNF, QNF, QPS
       REAL(KIND=REAL64), INTENT(OUT) :: APP, AQQ, AQP
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE DGRAM
  END INTERFACE
  INTERFACE
     SUBROUTINE DLJTU2(A11, A22, A21, CS, TN, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       REAL(KIND=REAL64), INTENT(INOUT) :: A11, A22, A21
       REAL(KIND=REAL64), INTENT(OUT) :: CS, TN
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DLJTU2
  END INTERFACE
  INTERFACE
     SUBROUTINE DLJTV2(A11, A22, A21, CH, TH, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       REAL(KIND=REAL64), INTENT(INOUT) :: A11, A22, A21, CH
       REAL(KIND=REAL64), INTENT(OUT) :: TH
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DLJTV2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE DRTT(M, X, Y, CS, TN, GX, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       REAL(KIND=REAL64), INTENT(INOUT) :: X(M), Y(M), GX
       REAL(KIND=REAL64), INTENT(IN) :: CS, TN
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DRTT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE DRTH(M, X, Y, CH, TH, GX, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       REAL(KIND=REAL64), INTENT(INOUT) :: X(M), Y(M), GX
       REAL(KIND=REAL64), INTENT(IN) :: CH, TH
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DRTH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE DSCALG(M, N, G, LDG, GX, GS, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,N), GX
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE DSCALG
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q, IX(N)
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N), V(LDV,N), SV(N), GX, TOL
  REAL(KIND=K), INTENT(OUT) :: WRK(M,N)
  INTEGER, INTENT(INOUT) :: GS, INFO
  REAL(KIND=K) :: QPS, APP, AQQ, AQP, C, S, T
  INTEGER :: I, J, L, O
#ifndef NDEBUG
  IF ((INFO .LT. 0) .OR. (INFO .GT. 3)) INFO = -15
  IF (TOL .LT. ZERO) INFO = -12
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
  QPS = DSDP(M, G(1,IX(Q)), G(1,IX(P)), SV(Q), SV(P), I)
#ifndef NDEBUG
  IF (I .LT. 0) THEN
     INFO = -3
     RETURN
  END IF
#endif
  S = ABS(QPS)
#ifndef NDEBUG
  IF (.NOT. (S .LE. HUGE(S))) THEN
     INFO = -14
     RETURN
  END IF
#endif
  C = ONE
  T = ZERO
  IF (S .LT. TOL) THEN
     TOL = ZERO
     INFO = 0
     GOTO 9
  END IF
  J = 0
  CALL DGRAM(SV(P), SV(Q), QPS, APP, AQQ, AQP, J)
#ifndef NDEBUG
  IF (J .LE. -HUGE(J)) THEN
     INFO = -7
     RETURN
  END IF
#endif
  I = 0
  QPS = GX
  IF (L .EQ. 0) THEN
     CALL DLJTU2(APP, AQQ, AQP, C, T, I)
     IF (I .GT. 0) THEN
        J = 0
        CALL DRTT(N, V(1,IX(P)), V(1,IX(Q)), C, T, QPS, J)
#ifndef NDEBUG
        IF (J .LT. 0) THEN
           INFO = -5
           RETURN
        END IF
#endif
        J = 1
        CALL DRTT(M, G(1,IX(P)), G(1,IX(Q)), C, T, QPS, J)
#ifndef NDEBUG
        IF (J .LT. 0) THEN
           INFO = -13
           RETURN
        END IF
#endif
     END IF
  ELSE ! hyp
     C = CUTOFF
     CALL DLJTV2(APP, AQQ, AQP, C, T, I)
     IF (I .GT. 0) THEN
        J = 0
        CALL DRTH(N, V(1,IX(P)), V(1,IX(Q)), C, T, QPS, J)
#ifndef NDEBUG
        IF (J .LT. 0) THEN
           INFO = -5
           RETURN
        END IF
#endif
        J = 1
        CALL DRTH(M, G(1,IX(P)), G(1,IX(Q)), C, T, QPS, J)
#ifndef NDEBUG
        IF (J .LT. 0) THEN
           INFO = -13
           RETURN
        END IF
#endif
     END IF
  END IF
  TOL = AQP
  IF (I .EQ. 0) THEN
     INFO = 1
  ELSE IF (I .LT. 0) THEN
     INFO = -8
  ELSE ! I > 0
     IF (O .EQ. 0) THEN
        ! S = ABS(A21 / (SV(P) * SV(Q)))
        ! norm update, trig:
        ! SQRT(SV(P) + TN * (S * SV(Q))) * SQRT(SV(P))
        ! SQRT(SV(Q) - TN * (S * SV(P))) * SQRT(SV(Q))
        ! norm update, hyp:
        ! SQRT(SV(P) + TH * (S * SV(Q))) * SQRT(SV(P))
        ! SQRT(SV(Q) + TH * (S * SV(P))) * SQRT(SV(Q))
        APP = S * SV(Q)
        AQQ = S * SV(P)
        IF (L .EQ. 0) AQP = -AQP
        APP = SQRT(DFMA(TOL, APP, SV(P)))
        AQQ = SQRT(DFMA(AQP, AQQ, SV(Q)))
        SV(P) = APP * SQRT(SV(P))
        SV(Q) = AQQ * SQRT(SV(Q))
     ELSE ! SLOW
        SV(P) = DNRMF(M, G(1,IX(P)))
        SV(Q) = DNRMF(M, G(1,IX(Q)))
     END IF
     INFO = I + 1
     IF (QPS .GT. GX) THEN
        GX = QPS
        I = -INFO
        CALL DSCALG(M, N, G, LDG, GX, GS, I)
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
9 WRK(P,Q) = C
  WRK(Q,P) = T
END SUBROUTINE DTRNSF

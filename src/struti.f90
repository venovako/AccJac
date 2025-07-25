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
SUBROUTINE STRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, IX, WRK, RWRK, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  IMPLICIT NONE
#include "cr.f90"
  INTERFACE
     FUNCTION SSDP(M, X, Y, MX, MY, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       INTEGER, INTENT(INOUT) :: INFO
       REAL(KIND=REAL32), INTENT(IN) :: X(M), Y(M), MX, MY
       REAL(KIND=REAL32) :: SSDP
     END FUNCTION SSDP
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE SGRAM(PNF, QNF, QPS, APP, AQQ, AQP, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       REAL(KIND=REAL32), INTENT(IN) :: PNF, QNF, QPS
       REAL(KIND=REAL32), INTENT(OUT) :: APP, AQQ, AQP
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE SGRAM
  END INTERFACE
  INTERFACE
     SUBROUTINE SLJTU2(A11, A22, A21, CS, TN, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       REAL(KIND=REAL32), INTENT(INOUT) :: A11, A22, A21
       REAL(KIND=REAL32), INTENT(OUT) :: CS, TN
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE SLJTU2
  END INTERFACE
  INTERFACE
     SUBROUTINE SLJTV2(A11, A22, A21, CH, TH, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       REAL(KIND=REAL32), INTENT(INOUT) :: A11, A22, A21, CH
       REAL(KIND=REAL32), INTENT(OUT) :: TH
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE SLJTV2
  END INTERFACE
  INTERFACE
#ifdef _OPENMP
     SUBROUTINE SSCALG(M, N, G, LDG, GX, GS, INFO)
#else
     PURE SUBROUTINE SSCALG(M, N, G, LDG, GX, GS, INFO)
#endif
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
       REAL(KIND=REAL32), INTENT(INOUT) :: G(LDG,N), GX
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE SSCALG
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL32
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q, IX(N)
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N), V(LDV,N), SV(N), GX, TOL, WRK(M,N), RWRK(N)
  INTEGER, INTENT(INOUT) :: GS, INFO
  REAL(KIND=K) :: QPS, APP, AQQ, AQP, C, S, T, CC, XX, YY
  INTEGER :: I, J, L, O
#ifndef NDEBUG
  IF ((INFO .LT. 0) .OR. (INFO .GT. 3)) INFO = -16
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
  J = IX(P)
  CC = RWRK(P)
  DO I = 1, M
     WRK(I,N) = SFMA(G(I,J), CC, WRK(I,P))
  END DO
  IF (IAND(INFO, 2) .EQ. 0) THEN
     I = 1
  ELSE ! SLOW
     I = 0
  END IF
  QPS = SSDP(M, G(1,IX(Q)), WRK(1,N), SV(Q), SV(P), I)
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
  T = QPS
  QPS = GX
  IF (S .LT. TOL) THEN
     TOL = ZERO
     INFO = 0
     GOTO 9
  END IF
  QPS = T
  J = 0
  CALL SGRAM(SV(P), SV(Q), QPS, APP, AQQ, AQP, J)
#ifndef NDEBUG
  IF (J .LE. -HUGE(J)) THEN
     INFO = -7
     RETURN
  END IF
#endif
  I = 0
  IF (IAND(INFO, 1) .EQ. 0) THEN
     CALL SLJTU2(APP, AQQ, AQP, C, T, I)
     IF (I .GT. 0) THEN
        O = IX(Q)
        AQQ = -T
        DO L = 1, M
           XX = WRK(L,P) ! ZZ
           YY = G(L,O)
           XX = SFMA(YY, T, XX) * C
           WRK(L,P) = XX
           QPS = MAX(QPS, ABS(XX))
           XX = WRK(L,N) ! XX
           YY = SFMA(XX, AQQ, YY) * C
           G(L,O) = YY
           QPS = MAX(QPS, ABS(YY))
        END DO
        J = IX(P)
        DO L = 1, N
           XX = V(L,J)
           YY = V(L,O)
           V(L,J) = SFMA(YY, T, XX) * C
           V(L,O) = SFMA(XX, AQQ, YY) * C
        END DO
     END IF
  ELSE ! hyp
     C = CUTOFF
     CALL SLJTV2(APP, AQQ, AQP, C, T, I)
     IF (I .GT. 0) THEN
        O = IX(Q)
        DO L = 1, M
           XX = WRK(L,P) ! ZZ
           YY = G(L,O)
           XX = SFMA(YY, T, XX) * C
           WRK(L,P) = XX
           QPS = MAX(QPS, ABS(XX))
           XX = WRK(L,N) ! XX
           YY = SFMA(XX, T, YY) * C
           G(L,O) = YY
           QPS = MAX(QPS, ABS(YY))
        END DO
        J = IX(P)
        DO L = 1, N
           XX = V(L,J)
           YY = V(L,O)
           V(L,J) = SFMA(YY, T, XX) * C
           V(L,O) = SFMA(XX, T, YY) * C
        END DO
     END IF
  END IF
  TOL = AQP
  IF (I .EQ. 0) THEN
     INFO = 1
  ELSE IF (I .LT. 0) THEN
     INFO = -5
     RETURN
  ELSE ! I > 0
     CC = CC * C
     ! S = ABS(A21 / (SV(P) * SV(Q)))
     ! norm update, trig:
     ! SQRT(SV(P) + TN * (S * SV(Q))) * SQRT(SV(P))
     ! SQRT(SV(Q) - TN * (S * SV(P))) * SQRT(SV(Q))
     ! norm update, hyp:
     ! SQRT(SV(P) + TH * (S * SV(Q))) * SQRT(SV(P))
     ! SQRT(SV(Q) + TH * (S * SV(P))) * SQRT(SV(Q))
     APP = S * SV(Q)
     AQQ = S * SV(P)
     IF (IAND(INFO, 1) .EQ. 0) AQP = -AQP
     APP = SQRT(SFMA(TOL, APP, SV(P)))
     AQQ = SQRT(SFMA(AQP, AQQ, SV(Q)))
     SV(P) = APP * SQRT(SV(P))
     SV(Q) = AQQ * SQRT(SV(Q))
     INFO = I + 1
     IF (QPS .GT. GX) THEN
        GX = QPS
        I = -INFO
        CALL SSCALG(M, N, G, LDG, GX, GS, I)
        IF (I .GT. 0) THEN
           I = -I
           DO J = 1, N
              SV(J) = SCALE(SV(J), I)
           END DO
           DO J = 1, N-1
              DO L = 1, M
                 WRK(L,J) = SCALE(WRK(L,J), I)
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
        G(I,J) = SFMA(G(I,J), CC, WRK(I,P))
        QPS = MAX(QPS, ABS(G(I,J)))
     END DO
     IF (QPS .GT. GX) THEN
        GX = QPS
        I = -2
        CALL SSCALG(M, N, G, LDG, GX, GS, I)
        IF (I .GT. 0) THEN
           I = -I
           DO J = 1, N
              SV(J) = SCALE(SV(J), I)
           END DO
           DO J = 1, N-1
              DO L = 1, M
                 WRK(L,J) = SCALE(WRK(L,J), I)
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
END SUBROUTINE STRUTI

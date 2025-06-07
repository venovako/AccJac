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
SUBROUTINE XTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, IX, WRK, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
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
  INTERFACE
     FUNCTION XSDP(M, X, Y, MX, MY, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       INTEGER, INTENT(INOUT) :: INFO
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: X(M), Y(M), MX, MY
       REAL(KIND=c_long_double) :: XSDP
#else
       REAL(KIND=REAL128), INTENT(IN) :: X(M), Y(M), MX, MY
       REAL(KIND=REAL128) :: XSDP
#endif
     END FUNCTION XSDP
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XGRAM(PNF, QNF, QPS, APP, AQQ, AQP, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: PNF, QNF, QPS
       REAL(KIND=c_long_double), INTENT(OUT) :: APP, AQQ, AQP
#else
       REAL(KIND=REAL128), INTENT(IN) :: PNF, QNF, QPS
       REAL(KIND=REAL128), INTENT(OUT) :: APP, AQQ, AQP
#endif
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE XGRAM
  END INTERFACE
  INTERFACE
     SUBROUTINE XLJTU2(A11, A22, A21, CS, TN, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A11, A22, A21
       REAL(KIND=c_long_double), INTENT(OUT) :: CS, TN
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A11, A22, A21
       REAL(KIND=REAL128), INTENT(OUT) :: CS, TN
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XLJTU2
  END INTERFACE
  INTERFACE
     SUBROUTINE XLJTV2(A11, A22, A21, CH, TH, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A11, A22, A21
       REAL(KIND=c_long_double), INTENT(OUT) :: CH, TH
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A11, A22, A21
       REAL(KIND=REAL128), INTENT(OUT) :: CH, TH
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XLJTV2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XSCALG(M, N, G, LDG, GX, GS, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: G(LDG,N), GX
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: G(LDG,N), GX
#endif
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE XSCALG
  END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q, IX(N)
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N), V(LDV,N), SV(N), GX, TOL, WRK(M,N+1)
  INTEGER, INTENT(INOUT) :: GS, INFO
  REAL(KIND=K) :: QPS, APP, AQQ, AQP, C, S, T, CC, XX, YY
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
  J = IX(P)
  CC = WRK(P,N+1)
  DO I = 1, M
     WRK(I,N) = G(I,J) * CC + WRK(I,P)
  END DO
  IF (IAND(INFO, 2) .EQ. 0) THEN
     I = 1
  ELSE ! SLOW
     I = 0
  END IF
  QPS = XSDP(M, G(1,IX(Q)), WRK(1,N), SV(Q), SV(P), I)
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
  CALL XGRAM(SV(P), SV(Q), QPS, APP, AQQ, AQP, J)
#ifndef NDEBUG
  IF (J .LE. -HUGE(J)) THEN
     INFO = -7
     RETURN
  END IF
#endif
  I = 0
  IF (IAND(INFO, 1) .EQ. 0) THEN
     CALL XLJTU2(APP, AQQ, AQP, C, T, I)
     IF (I .GT. 0) THEN
        O = IX(Q)
        DO L = 1, M
           XX = WRK(L,P) ! ZZ
           YY = G(L,O)
           WRK(L,P) = (XX + YY * T) * C
           XX = WRK(L,N) ! XX
           G(L,O) = (YY - XX * T) * C
           QPS = MAX(QPS, ABS(G(L,O)))
        END DO
        J = IX(P)
        DO L = 1, N
           XX = V(L,J)
           YY = V(L,O)
           V(L,J) = (XX + YY * T) * C
           V(L,O) = (YY - XX * T) * C
        END DO
     END IF
  ELSE ! hyp
     CALL XLJTV2(APP, AQQ, AQP, C, T, I)
     IF (I .GT. 0) THEN
        O = IX(Q)
        DO L = 1, M
           XX = WRK(L,P) ! ZZ
           YY = G(L,O)
           WRK(L,P) = (YY * T + XX) * C
           XX = WRK(L,N) ! XX
           G(L,O) = (XX * T + YY) * C
           QPS = MAX(QPS, ABS(G(L,O)))
        END DO
        J = IX(P)
        DO L = 1, N
           XX = V(L,J)
           YY = V(L,O)
           V(L,J) = (YY * T + XX) * C
           V(L,O) = (XX * T + YY) * C
        END DO
     END IF
  END IF
  TOL = AQP
  IF (I .EQ. 0) THEN
     INFO = 1
  ELSE IF (I .LT. 0) THEN
     INFO = -8
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
     APP = SQRT(XFMA(TOL, APP, SV(P)))
     AQQ = SQRT(XFMA(AQP, AQQ, SV(Q)))
     SV(P) = APP * SQRT(SV(P))
     SV(Q) = AQQ * SQRT(SV(Q))
     INFO = I + 1
     IF (QPS .GT. GX) THEN
        GX = QPS
        I = -INFO
        CALL XSCALG(M, N, G, LDG, GX, GS, I)
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
        G(I,J) = G(I,J) * CC + WRK(I,P)
        QPS = MAX(QPS, ABS(G(I,J)))
     END DO
     IF (QPS .GT. GX) THEN
        GX = QPS
        I = -2
        CALL XSCALG(M, N, G, LDG, GX, GS, I)
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
  WRK(P,N+1) = CC
END SUBROUTINE XTRUTI

! IN: INFO & 1: sin => tan
!     INFO & 2: hyp
!OUT: INFO = 0: notransf
!     INFO = 1: swap only
!     INFO = 2: transf, no downscaling of G and SV
!     INFO = 3: transf with downscaling of G and SV
SUBROUTINE XTRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE
  INTERFACE
     FUNCTION XSDP(M, X, Y, MX, MY, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       REAL(KIND=c_long_double), INTENT(IN) :: X(M), Y(M), MX, MY
       INTEGER, INTENT(INOUT) :: INFO
       REAL(KIND=c_long_double) :: XSDP
     END FUNCTION XSDP
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XGRAM(PNF, QNF, QPS, APP, AQQ, AQP, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN) :: PNF, QNF, QPS
       REAL(KIND=c_long_double), INTENT(OUT) :: APP, AQQ, AQP
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE XGRAM
  END INTERFACE
  INTERFACE
     SUBROUTINE XLJU2(A11, A22, A21, CS, SN, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN) :: A11, A22, A21
       REAL(KIND=c_long_double), INTENT(OUT) :: CS, SN
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XLJU2
  END INTERFACE
  INTERFACE
     SUBROUTINE XLJV2(A11, A22, A21, CH, SH, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN) :: A11, A22, A21
       REAL(KIND=c_long_double), INTENT(OUT) :: CH, SH
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XLJV2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XRTVT(N, X, Y, CS, SN, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N
       REAL(KIND=c_long_double), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=c_long_double), INTENT(IN) :: CS, SN
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XRTVT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XRTVH(N, X, Y, CH, SH, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N
       REAL(KIND=c_long_double), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=c_long_double), INTENT(IN) :: CH, SH
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XRTVH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XROTT(M, X, Y, CS, SN, GX, MX, MY, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       REAL(KIND=c_long_double), INTENT(INOUT) :: X(M), Y(M), GX, MX, MY
       REAL(KIND=c_long_double), INTENT(IN) :: CS, SN
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XROTT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XROTH(M, X, Y, CH, SH, GX, MX, MY, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       REAL(KIND=c_long_double), INTENT(INOUT) :: X(M), Y(M), GX, MX, MY
       REAL(KIND=c_long_double), INTENT(IN) :: CH, SH
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XROTH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XSCALG(M, N, G, LDG, GX, GS, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
       REAL(KIND=c_long_double), INTENT(INOUT) :: G(LDG,N), GX
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE XSCALG
  END INTERFACE
  INTEGER, PARAMETER :: K = c_long_double
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N), V(LDV,N), SV(N), GX
  REAL(KIND=K), INTENT(IN) :: TOL
  INTEGER, INTENT(INOUT) :: GS, INFO
  REAL(KIND=K) :: QPS, APP, AQQ, AQP, C, S, T
  INTEGER :: I, J, L
  IF ((INFO .LT. 0) .OR. (INFO .GT. 3)) INFO = -13
  IF (TOL .LT. ZERO) INFO = -12
  IF ((Q .LE. 0) .OR. (Q .GT. N)) INFO = -11
  IF ((P .LE. 0) .OR. (P .GT. N)) INFO = -10
  IF (GX .LT. ZERO) INFO = -8
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF ((N .LT. 0) .OR. (N .GT. M)) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (M .EQ. 0) RETURN
  I = 0
  QPS = XSDP(M, G(1,Q), G(1,P), SV(Q), SV(P), I)
  IF (I .NE. 0) THEN
     INFO = -3
     RETURN
  END IF
  T = ABS(QPS)
  IF (.NOT. (T .LE. HUGE(T))) THEN
     INFO = -3
     RETURN
  END IF
  L = INFO
  IF (T .LT. TOL) THEN
     IF ((IAND(L, 2) .EQ. 0) .AND. (SV(P) .LT. SV(Q))) THEN
        DO I = 1, M
           T = G(I,P)
           G(I,P) = G(I,Q)
           G(I,Q) = T
        END DO
        DO I = 1, N
           T = V(I,P)
           V(I,P) = V(I,Q)
           V(I,Q) = T
        END DO
        T = SV(P)
        SV(P) = SV(Q)
        SV(Q) = T
        INFO = 1
     ELSE ! no swap
        INFO = 0
     END IF
     RETURN
  ELSE ! transform
     INFO = 2
  END IF
  J = 0
  CALL XGRAM(SV(P), SV(Q), QPS, APP, AQQ, AQP, J)
  IF (J .LE. -HUGE(J)) THEN
     INFO = -7
     RETURN
  END IF
  J = IAND(L, 2)
  I = IAND(L, 1)
  T = GX
  IF (J .EQ. 0) THEN
     CALL XLJU2(APP, AQQ, AQP, C, S, I)
     CALL XROTT(M, G(1,P), G(1,Q), C, S, T, SV(P), SV(Q), I)
     IF (J .EQ. 0) CALL XRTVT(N, V(1,P), V(1,Q), C, S, I)
  ELSE ! hyp
     CALL XLJV2(APP, AQQ, AQP, C, S, I)
     CALL XROTH(M, G(1,P), G(1,Q), C, S, T, SV(P), SV(Q), I)
     CALL XRTVH(N, V(1,P), V(1,Q), C, S, I)
  END IF
  IF (I .LT. 0) THEN
     INFO = -8
  ELSE IF (IAND(I, 4) .NE. 0) THEN
     IF (IAND(I, 8) .EQ. 0) THEN
        INFO = 0
     ELSE ! swap
        INFO = 1
     END IF
  ELSE IF (T .GT. GX) THEN
     GX = T
     I = 1
     CALL XSCALG(M, N, G, LDG, GX, GS, I)
     IF (I .LT. 0) THEN
        INFO = -9
        RETURN
     END IF
     IF (I .GT. 0) THEN
        I = -I
        DO J = 1, N
           SV(J) = SCALE(SV(J), I)
        END DO
        INFO = 3
     END IF
  END IF
END SUBROUTINE XTRANS

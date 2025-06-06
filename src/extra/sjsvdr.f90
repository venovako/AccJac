!  IN: GS = max sweeps, INFO = 0 or 1 (sin => tan) OR 2 (dsc/asc)
! OUT: GS: backscale SV by 2**-GS, INFO: #sweeps
SUBROUTINE SJSVDR(M, N, G, LDG, V, LDV, JPOS, SV, WRK, GS, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE SSCALG(M, N, G, LDG, GX, GS, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
       REAL(KIND=REAL32), INTENT(INOUT) :: G(LDG,N), GX
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE SSCALG
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE SINISV(M, N, G, LDG, V, LDV, JPOS, SV, WRK, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS
       REAL(KIND=REAL32), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=REAL32), INTENT(OUT) :: V(LDV,N), SV(N), WRK(M,N)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE SINISV
  END INTERFACE
  INTERFACE
     SUBROUTINE STRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q
       REAL(KIND=REAL32), INTENT(INOUT) :: G(LDG,N), V(LDV,N), SV(N), GX
       REAL(KIND=REAL32), INTENT(IN) :: TOL
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE STRANS
  END INTERFACE
  INTERFACE
     SUBROUTINE STRACK(N, SV, GX, GS, SWP, NTR)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, GS, SWP, NTR
       REAL(KIND=REAL32), INTENT(IN) :: SV(N), GX
     END SUBROUTINE STRACK
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL32
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, EPS = EPSILON(EPS) / 2
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(OUT) :: V(LDV,N), SV(N), WRK(M,N)
  INTEGER, INTENT(INOUT) :: GS, INFO
  REAL(KIND=K) :: GX, TOL, X
  INTEGER :: P, Q, R, S, T, W
  IF ((INFO .LT. 0) .OR. (INFO .GT. 7)) INFO = -11
  IF (GS .LT. 0) INFO = -10
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -7
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF ((N .LT. 0) .OR. (N .GT. M)) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) RETURN
  S = GS
  ! prescale G
  GX = ZERO
  GS = 0
  R = 0
  CALL SSCALG(M, N, G, LDG, GX, GS, R)
  IF (R .LT. 0) THEN
     INFO = -3
     RETURN
  END IF
  ! init SV, V; sort SV, G
  R = IAND(INFO, 2)
  CALL SINISV(M, N, G, LDG, V, LDV, JPOS, SV, WRK, R)
  IF (R .LT. 0) THEN
     INFO = -9
     RETURN
  END IF
  TOL = M
  TOL = SQRT(TOL) * EPS
  CALL STRACK(N, SV, GX, GS, -R, S)
  DO R = 1, S
     T = 0
     ! the first diagonal block
     DO P = 1, JPOS-1
        X = ZERO
        W = 0
        DO Q = P, JPOS
           IF (SV(Q) .GT. X) THEN
              X = SV(Q)
              W = Q
           END IF
        END DO
        IF (W .GT. P) THEN
           DO Q = 1, M
              X = G(Q,P)
              G(Q,P) = G(Q,W)
              G(Q,W) = X
           END DO
           DO Q = 1, N
              X = V(Q,P)
              V(Q,P) = V(Q,W)
              V(Q,W) = X
           END DO
           X = SV(P)
           SV(P) = SV(W)
           SV(W) = X
           T = T + 1
        END IF
        DO Q = P+1, JPOS
           W = IAND(INFO, 1)
           CALL STRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, W)
           SELECT CASE (W)
           CASE (0)
              CONTINUE
           CASE (1,2,3)
              T = T + 1
           CASE DEFAULT
              INFO = -5
              RETURN
           END SELECT
        END DO
     END DO
     ! the off-diagonal block (hyp)
     DO P = 1, JPOS
        DO Q = JPOS+1, N
           W = IOR(IAND(INFO, 1), 2)
           CALL STRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, W)
           SELECT CASE (W)
           CASE (0)
              CONTINUE
           CASE (1,2,3)
              T = T + 1
           CASE DEFAULT
              INFO = -5
              RETURN
           END SELECT
        END DO
     END DO
     ! the second diagonal block
     IF (IAND(INFO, 2) .EQ. 0) THEN
        DO P = JPOS+1, N-1
           X = ZERO
           W = 0
           DO Q = P, N
              IF (SV(Q) .GT. X) THEN
                 X = SV(Q)
                 W = Q
              END IF
           END DO
           IF (W .GT. P) THEN
              DO Q = 1, M
                 X = G(Q,P)
                 G(Q,P) = G(Q,W)
                 G(Q,W) = X
              END DO
              DO Q = 1, N
                 X = V(Q,P)
                 V(Q,P) = V(Q,W)
                 V(Q,W) = X
              END DO
              X = SV(P)
              SV(P) = SV(W)
              SV(W) = X
              T = T + 1
           END IF
           DO Q = P+1, N
              W = IAND(INFO, 1)
              CALL STRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, W)
              SELECT CASE (W)
              CASE (0)
                 CONTINUE
              CASE (1,2,3)
                 T = T + 1
              CASE DEFAULT
                 INFO = -5
                 RETURN
              END SELECT
           END DO
        END DO
     ELSE ! asc
        DO Q = N, JPOS+2, -1
           X = ZERO
           W = 0
           DO P = Q, JPOS+1, -1
              IF (SV(P) .GT. X) THEN
                 X = SV(P)
                 W = P
              END IF
           END DO
           IF (W .LT. Q) THEN
              DO P = 1, M
                 X = G(P,Q)
                 G(P,Q) = G(P,W)
                 G(P,W) = X
              END DO
              DO P = 1, N
                 X = V(P,Q)
                 V(P,Q) = V(P,W)
                 V(P,W) = X
              END DO
              X = SV(Q)
              SV(Q) = SV(W)
              SV(W) = X
              T = T + 1
           END IF
           DO P = Q-1, JPOS+1, -1
              W = IAND(INFO, 1)
              CALL STRANS(M, N, G, LDG, V, LDV, SV, GX, GS, Q, P, TOL, W)
              SELECT CASE (W)
              CASE (0)
                 CONTINUE
              CASE (1,2,3)
                 T = T + 1
              CASE DEFAULT
                 INFO = -5
                 RETURN
              END SELECT
           END DO
        END DO
     END IF
     CALL STRACK(N, SV, GX, GS, R, T)
     IF (T .EQ. 0) EXIT
  END DO
  ! rescale U
  IF (R .LE. S) THEN
     DO Q = 1, N
        IF (.NOT. (SV(Q) .GT. ZERO)) THEN
           INFO = -8
           RETURN
        END IF
        IF (SV(Q) .NE. ONE) THEN
           DO P = 1, M
              G(P,Q) = G(P,Q) / SV(Q)
           END DO
        END IF
     END DO
  END IF
  INFO = R
END SUBROUTINE SJSVDR

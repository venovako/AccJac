!  IN: GS = max sweeps, INFO = 0 or 1 (SLOW)
! OUT: GS: backscale SV by 2**-GS, INFO: #sweeps
! *** FAST IS NOT RECOMMENDED FOR NOW; USE SLOW ***
SUBROUTINE DJSVDF(M, N, G, LDG, V, LDV, JPOS, SV, GS, IX, WRK, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL64
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE DINISX(M, N, G, LDG, V, LDV, SV, IX, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV
       REAL(KIND=REAL64), INTENT(IN) :: G(LDG,N)
       REAL(KIND=REAL64), INTENT(OUT) :: V(LDV,N), SV(N)
       INTEGER, INTENT(OUT) :: IX(N)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DINISX
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
  INTERFACE
     PURE SUBROUTINE DPRCYC(M, N, G, LDG, JPOS, SV, IX, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, JPOS
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=REAL64), INTENT(OUT) :: SV(N)
       INTEGER, INTENT(INOUT) :: IX(N), INFO
     END SUBROUTINE DPRCYC
  END INTERFACE
  INTERFACE
     SUBROUTINE DTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, IX, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q, IX(N)
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,N), V(LDV,N), SV(N), GX, TOL
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE DTRNSF
  END INTERFACE
  INTERFACE
     SUBROUTINE DTRACK(N, SV, GX, GS, SWP, NTR)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, GS, SWP, NTR
       REAL(KIND=REAL64), INTENT(IN) :: SV(N), GX
     END SUBROUTINE DTRACK
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, EPS = EPSILON(EPS) / 2
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(OUT) :: V(LDV,N), SV(N), WRK(M,N)
  INTEGER, INTENT(OUT) :: IX(N)
  INTEGER, INTENT(INOUT) :: GS, INFO
  REAL(KIND=K) :: GX, TOL, X
  INTEGER(KIND=INT64) :: TT
  INTEGER :: O, P, Q, R, S, T, W
  IF ((INFO .LT. 0) .OR. (INFO .GT. 1)) INFO = -12
  IF (GS .LT. 0) INFO = -9
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -7
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. N) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) THEN
     INFO = 0
     RETURN
  END IF
  S = GS
  GS = 0
  TT = 0_INT64
  IF (INFO .EQ. 0) THEN
     O = -1
  ELSE ! SLOW
     O = 0
  END IF
  CALL DSCALG(M, N, G, LDG, GX, GS, O)
  IF (O .LT. 0) THEN
     INFO = -3
     GOTO 9
  END IF
  R = INFO
  CALL DINISX(M, N, G, LDG, V, LDV, SV, IX, R)
  IF (R .NE. 0) THEN
     INFO = -10
     GOTO 9
  END IF
  IF (INFO .NE. 0) CALL DTRACK(N, SV, GX, GS, R, -S)
  TOL = M
  TOL = SQRT(TOL) * EPS  
  DO R = 1, S
     IF (INFO .EQ. 0) THEN
        O = 1
     ELSE ! SLOW
        O = 0
     END IF
     CALL DPRCYC(M, N, G, LDG, JPOS, SV, IX, O)
     IF (O .LT. 0) THEN
        INFO = -8
        GOTO 9
     END IF
     T = 0
     ! the first diagonal block
     DO P = 1, JPOS-1
        IF (P .GT. 1) THEN
           X = ZERO
           W = 0
           DO Q = P, JPOS
              IF (SV(Q) .GT. X) THEN
                 X = SV(Q)
                 W = Q
              END IF
           END DO
           IF (W .GT. P) THEN
              X = SV(P)
              SV(P) = SV(W)
              SV(W) = X
              O = IX(P)
              IX(P) = IX(W)
              IX(W) = O
              T = T + 1
           END IF
        END IF
        DO Q = P+1, JPOS
           X = TOL
           IF (INFO .EQ. 0) THEN
              O = 0
           ELSE ! SLOW
              O = 2
           END IF
           CALL DTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, X, IX, O)
           SELECT CASE (O)
           CASE (0,1)
              CONTINUE
           CASE (2,3,6,7)
              T = T + 1
              TT = TT + 1_INT64
           CASE DEFAULT
              INFO = -5
              GOTO 9
           END SELECT
        END DO
     END DO
     ! the off-diagonal block (hyp)
     DO P = 1, JPOS
        DO Q = JPOS+1, N
           X = TOL
           IF (INFO .EQ. 0) THEN
              O = 1
           ELSE ! SLOW
              O = 3
           END IF
           CALL DTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, X, IX, O)
           SELECT CASE (O)
           CASE (0,1)
              CONTINUE
           CASE (2,3,6,7)
              T = T + 1
              TT = TT + 1_INT64
           CASE DEFAULT
              INFO = -5
              GOTO 9
           END SELECT
        END DO
     END DO
     ! the second diagonal block
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
           X = SV(P)
           SV(P) = SV(W)
           SV(W) = X
           O = IX(P)
           IX(P) = IX(W)
           IX(W) = O
           T = T + 1
        END IF
        DO Q = P+1, N
           X = TOL
           IF (INFO .EQ. 0) THEN
              O = 0
           ELSE ! SLOW
              O = 2
           END IF
           CALL DTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, X, IX, O)
           SELECT CASE (O)
           CASE (0,1)
              CONTINUE
           CASE (2,3,6,7)
              T = T + 1
              TT = TT + 1_INT64
           CASE DEFAULT
              INFO = -5
              GOTO 9
           END SELECT
        END DO
     END DO
     IF (INFO .NE. 0) CALL DTRACK(N, SV, GX, GS, R, -T)
     IF (T .EQ. 0) EXIT
  END DO
  IF (R .LE. S) THEN
     ! permute V
     DO Q = 1, N
        DO P = 1, N
           WRK(P,Q) = V(P,IX(Q))
        END DO
     END DO
     DO Q = 1, N
        DO P = 1, N
           V(P,Q) = WRK(P,Q)
        END DO
     END DO
     ! permute and rescale U
     DO Q = 1, N
        IF (.NOT. (SV(Q) .GT. ZERO)) THEN
           INFO = -11
           GOTO 9
        END IF
        IF (SV(Q) .NE. ONE) THEN
           IF (INFO .EQ. 0) THEN
              X = ONE / SV(Q)
              DO P = 1, M
                 WRK(P,Q) = G(P,IX(Q)) * X
              END DO
           ELSE ! SLOW
              DO P = 1, M
                 WRK(P,Q) = G(P,IX(Q)) / SV(Q)
              END DO
           END IF
        ELSE ! no division
           DO P = 1, M
              WRK(P,Q) = G(P,IX(Q))
           END DO
        END IF
     END DO
     DO Q = 1, N
        DO P = 1, M
           G(P,Q) = WRK(P,Q)
        END DO
     END DO
  END IF
  INFO = R
9 WRK(1,1) = TRANSFER(TT, ZERO)
END SUBROUTINE DJSVDF

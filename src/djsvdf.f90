!  IN: GS = max sweeps, IX(1) = trace unit, INFO
! OUT: GS: backscale SV by 2**-GS, INFO: #sweeps
SUBROUTINE DJSVDF(M, N, G, LDG, V, LDV, JPOS, SV, GS, IX, WRK, RWRK, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL64
  IMPLICIT NONE
  INTERFACE
#ifdef _OPENMP
     SUBROUTINE DINISX(M, N, G, LDG, V, LDV, SV, IX, INFO)
#else
     PURE SUBROUTINE DINISX(M, N, G, LDG, V, LDV, SV, IX, INFO)
#endif
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
#ifdef _OPENMP
     SUBROUTINE DSCALG(M, N, G, LDG, GX, GS, INFO)
#else
     PURE SUBROUTINE DSCALG(M, N, G, LDG, GX, GS, INFO)
#endif
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,N), GX
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE DSCALG
  END INTERFACE
  INTERFACE
#ifdef _OPENMP
     SUBROUTINE DPRCYC(M, N, G, LDG, JPOS, SV, IX, WRK, RWRK, INFO)
#else
     PURE SUBROUTINE DPRCYC(M, N, G, LDG, JPOS, SV, IX, WRK, RWRK, INFO)
#endif
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, JPOS
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=REAL64), INTENT(OUT) :: SV(N), WRK(M,N), RWRK(N)
       INTEGER, INTENT(INOUT) :: IX(N), INFO
     END SUBROUTINE DPRCYC
  END INTERFACE
  INTERFACE
     SUBROUTINE DTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, IX, WRK, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q, IX(N)
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,N), V(LDV,N), SV(N), GX, TOL
       REAL(KIND=REAL64), INTENT(OUT) :: WRK(M,N)
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE DTRNSF
  END INTERFACE
  INTERFACE
     SUBROUTINE DTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, IX, WRK, RWRK, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q, IX(N)
       REAL(KIND=REAL64), INTENT(INOUT) :: G(LDG,N), V(LDV,N), SV(N), GX, TOL, WRK(M,N), RWRK(N)
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE DTRUTI
  END INTERFACE
  INTERFACE
     SUBROUTINE DTRACK(N, SV, GX, GS, SWP, NTR, U)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, GS, SWP, NTR, U
       REAL(KIND=REAL64), INTENT(IN) :: SV(N), GX
     END SUBROUTINE DTRACK
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, EPS = EPSILON(EPS) / 2
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(OUT) :: V(LDV,N), SV(N), WRK(M,N), RWRK(N)
  INTEGER, INTENT(INOUT) :: GS, IX(N), INFO
  REAL(KIND=K) :: GX, TOL, X, Y
  INTEGER(KIND=INT64) :: TT
  INTEGER :: L, O, P, Q, R, S, T, U, W
  IF ((INFO .LT. 0) .OR. (INFO .GT. 7)) INFO = -13
  IF (GS .LT. 0) INFO = -9
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -7
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. N) INFO = -1
  IF (INFO .LT. 0) RETURN
  S = GS
  GS = 0
  IF (N .EQ. 0) THEN
     INFO = 0
     RETURN
  END IF
  L = ISHFT(IAND(INFO, 6), -1)
  INFO = IAND(INFO, 1)
  U = IX(1)
  TT = 0_INT64
  O = -1
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
  CALL DTRACK(N, SV, GX, GS, R, -S, U)
  TOL = M
  TOL = SQRT(TOL) * EPS
  DO R = 1, S
     IF (INFO .EQ. 0) THEN
        O = 1
     ELSE ! SLOW
        O = 0
     END IF
     IF ((L .EQ. 2) .OR. (L .EQ. 3)) O = 2
     CALL DPRCYC(M, N, G, LDG, JPOS, SV, IX, WRK, RWRK, O)
     IF (O .LT. 0) THEN
        INFO = -8
        GOTO 9
     END IF
     T = 0
     Y = ZERO
     IF (IAND(L, 1) .EQ. 0) THEN
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
              IF (L .EQ. 0) THEN
                 CALL DTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, X, IX, WRK, O)
              ELSE ! L = 2
                 CALL DTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, X, IX, WRK, RWRK, O)
              END IF
              SELECT CASE (O)
              CASE (0,1)
                 CONTINUE
              CASE (4,5)
                 T = T + 1
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
              IF (L .EQ. 0) THEN
                 CALL DTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, X, IX, WRK, O)
              ELSE ! L = 2
                 CALL DTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, X, IX, WRK, RWRK, O)
              END IF
              SELECT CASE (O)
              CASE (0,1)
                 CONTINUE
              CASE (4,5)
                 T = T + 1
              CASE (2,3,6,7)
                 T = T + 1
                 TT = TT + 1_INT64
              CASE DEFAULT
                 INFO = -5
                 GOTO 9
              END SELECT
              Y = MAX(Y, ABS(X))
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
              IF (L .EQ. 0) THEN
                 CALL DTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, X, IX, WRK, O)
              ELSE ! L = 2
                 CALL DTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, X, IX, WRK, RWRK, O)
              END IF
              SELECT CASE (O)
              CASE (0,1)
                 CONTINUE
              CASE (4,5)
                 T = T + 1
              CASE (2,3,6,7)
                 T = T + 1
                 TT = TT + 1_INT64
              CASE DEFAULT
                 INFO = -5
                 GOTO 9
              END SELECT
           END DO
        END DO
     ELSE ! row-cyclic
        DO P = 1, N-1
           DO Q = P+1, N
              X = TOL
              IF ((P .LE. JPOS) .AND. (Q .GT. JPOS)) THEN
                 IF (INFO .EQ. 0) THEN
                    O = 1
                 ELSE ! SLOW
                    O = 3
                 END IF
              ELSE ! trig
                 IF (INFO .EQ. 0) THEN
                    O = 0
                 ELSE ! SLOW
                    O = 2
                 END IF
              END IF
              IF (L .EQ. 1) THEN
                 CALL DTRNSF(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, X, IX, WRK, O)
              ELSE ! L = 3
                 CALL DTRUTI(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, X, IX, WRK, RWRK, O)
              END IF
              SELECT CASE (O)
              CASE (0,1)
                 CONTINUE
              CASE (4,5)
                 T = T + 1
              CASE (2,3,6,7)
                 T = T + 1
                 TT = TT + 1_INT64
              CASE DEFAULT
                 INFO = -5
                 GOTO 9
              END SELECT
              IF ((P .LE. JPOS) .AND. (Q .GT. JPOS)) Y = MAX(Y, ABS(X))
           END DO
        END DO
     END IF
     CALL DTRACK(N, SV, Y, GS, R, -T, U)
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
9 RWRK(N) = REAL(TT, K)
END SUBROUTINE DJSVDF

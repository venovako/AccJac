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
  SELECT CASE (INFO)
  CASE (0)
     L = 2
     INFO = 0
  CASE (1)
     L = 2
     INFO = 1
  CASE (2)
     L = 4
     INFO = 0
  CASE (3)
     L = 4
     INFO = 0
  CASE DEFAULT
     INFO = -15
     RETURN
  END SELECT
  IF (N .LE. 1) THEN
     TS = 0
     TP = 0
  ELSE ! N >= 2
     TS = ORD(1,1)
     TP = ORD(2,1)
  END IF
  U = IX(1)
  TT = 0_INT64
  GX = ZERO
  O = -1
  CALL HSCALG(M, N, G, LDG, GX, GS, O)
  IF (O .LT. 0) THEN
     INFO = -3
     GOTO 9
  END IF
  R = INFO
  CALL HINISX(M, N, G, LDG, V, LDV, SV, IX, R)
  IF (R .NE. 0) THEN
     INFO = -10
     GOTO 9
  END IF
  CALL GTRACK(N, SV, GX, GS, R, -S, U)
  TOL = M
  TOL = GSQRT(TOL) * EPS
  DO R = 1, S
     IF (INFO .EQ. 0) THEN
        O = 1
     ELSE ! SLOW
        O = 0
     END IF
     CALL HPRCYC(M, N, G, LDG, JPOS, SV, IX, WRK, RWRK, O)
     IF (O .LT. 0) THEN
        INFO = -8
        GOTO 9
     END IF
     T = 0
     DO ST = 1, TS
        CALL JSTEP(L, N, TS, ST, TP, TBL, ORD, O)
        IF (O .NE. 0) THEN
           INFO = -14
           RETURN
        END IF
        W = 0
        X = ZERO
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(O,P,Q,Z) SHARED(M,N,G,LDG,V,LDV,JPOS,SV,IX,WRK,RWRK,ORD,INFO,TP,TOL,ST,U) REDUCTION(MAX:W,X)
        DO O = 1, TP
           P = ORD(1,O)
           Q = ORD(2,O)
           Z = CMPLX(TOL, REAL(ST, K), K)
           RWRK(O) = REAL(U, K)
           IF ((P .LE. JPOS) .AND. (Q .GT. JPOS)) THEN
              IF (INFO .EQ. 0) THEN
                 ORD(2,O) = 1
              ELSE ! SLOW
                 ORD(2,O) = 3
              END IF
           ELSE ! trig
              IF (INFO .EQ. 0) THEN
                 ORD(2,O) = 0
              ELSE ! SLOW
                 ORD(2,O) = 2
              END IF
           END IF
           CALL HTRNSP(M, N, G, LDG, V, LDV, SV, RWRK(O), P, Q, Z, IX, WRK, ORD(2,O))
           SELECT CASE (ORD(2,O))
           CASE (0,1)
              ORD(1,O) = 0
           CASE (2,3)
              ORD(1,O) = -O
           CASE DEFAULT
              ORD(1,O) = O
           END SELECT
           RWRK(O+TP) = ABS(AIMAG(Z))
           W = MAX(W, ORD(1,O))
           X = MAX(X, RWRK(O))
        END DO
        !$OMP END PARALLEL DO
        IF (W .GT. 0) THEN
           INFO = -5
           GOTO 9
        ELSE IF (W .LT. 0) THEN
           INFO = -13
           GOTO 9
        END IF
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(O) SHARED(ORD,TP) REDUCTION(+:W)
        DO O = 1, TP
           IF (ORD(1,O) .LT. 0) W = W + 1
        END DO
        !$OMP END PARALLEL DO
        T = T + W
        TT = TT + INT(W, INT64)
        IF (X .GT. GX) THEN
           GX = X
           O = -R - 1
           CALL HSCALG(M, N, G, LDG, GX, GS, O)
           IF (O .GT. 0) THEN
              O = -O
              !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(Q) SHARED(N,SV,O)
              DO Q = 1, N
                 SV(Q) = SCALE(SV(Q), O)
              END DO
              !$OMP END PARALLEL DO
           ELSE IF (O .LT. 0) THEN
              INFO = -12
              GOTO 9
           END IF
        END IF
     END DO
     CALL GTRACK(N, SV, GX, GS, R, -T, U)
     IF (T .EQ. 0) EXIT
  END DO
  IF (R .LE. S) THEN
     ! permute V
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(O,P,Q) SHARED(N,V,WRK,IX)
     DO Q = 1, N
        O = IX(Q)
        DO P = 1, N
           WRK(P,Q) = V(P,O)
        END DO
     END DO
     !$OMP END PARALLEL DO
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(P,Q) SHARED(N,V,WRK)
     DO Q = 1, N
        DO P = 1, N
           V(P,Q) = WRK(P,Q)
        END DO
     END DO
     !$OMP END PARALLEL DO
     ! permute and rescale U
     W = 0
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(O,P,Q,X,Z) SHARED(M,N,G,SV,WRK,IX,INFO) REDUCTION(MAX:W)
     DO Q = 1, N
        IF (.NOT. (SV(Q) .GT. ZERO)) THEN
           W = MAX(W, Q)
        ELSE IF (SV(Q) .NE. ONE) THEN
           IF (INFO .EQ. 0) THEN
              X = ONE / SV(Q)
              O = IX(Q)
              DO P = 1, M
                 Z = G(P,O)
                 WRK(P,Q) = CMPLX(REAL(Z) * X, AIMAG(Z) * X, K)
              END DO
           ELSE ! SLOW
              X = SV(Q)
              O = IX(Q)
              DO P = 1, M
                 Z = G(P,O)
                 WRK(P,Q) = CMPLX(REAL(Z) / X, AIMAG(Z) / X, K)
              END DO
           END IF
        ELSE ! no division
           O = IX(Q)
           DO P = 1, M
              WRK(P,Q) = G(P,O)
           END DO
        END IF
     END DO
     !$OMP END PARALLEL DO
     IF (W .NE. 0) THEN
        INFO = -11
        GOTO 9
     END IF
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(P,Q) SHARED(M,N,G,WRK)
     DO Q = 1, N
        DO P = 1, M
           G(P,Q) = WRK(P,Q)
        END DO
     END DO
     !$OMP END PARALLEL DO
  END IF
  INFO = R
9 RWRK(N) = REAL(TT, K)

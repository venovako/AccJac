#ifndef NDEBUG
  IF ((INFO .LT. 0) .OR. (INFO .GT. 3)) INFO = -14
  IF (REAL(TOL) .LT. ZERO) INFO = -11
  IF ((Q .LE. 0) .OR. (Q .GT. N)) INFO = -10
  IF ((P .LE. 0) .OR. (P .GT. N)) INFO = -9
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
#endif
  L = IAND(INFO, 1)
  O = IAND(INFO, 2)
  E = INT(GX)
  IF (O .EQ. 0) THEN
     I = 1
  ELSE ! SLOW
     I = 0
  END IF
  QPS = HSDP(M, G(1,IX(Q)), G(1,IX(P)), SV(Q), SV(P), I)
  IF (I .LT. 0) THEN
     IF (E .NE. INPUT_UNIT) WRITE (E,9) 'SDP:', INT(AIMAG(TOL)), ',', P, ',', Q, ',', I
     INFO = -3
     RETURN
  END IF
  S = CR_HYPOT(REAL(QPS), AIMAG(QPS))
  IF (.NOT. (S .LE. HUGE(S))) THEN
     IF (E .NE. INPUT_UNIT) WRITE (E,9) 'SDP!', INT(AIMAG(TOL)), ',', P, ',', Q, ',', I
     INFO = -12
     RETURN
  END IF
  C = ONE
  TR = ZERO
  TI = ZERO
  GX = ZERO
  IF (S .LT. REAL(TOL)) THEN
     TOL = ZERO
     INFO = 0
     GOTO 8
  END IF
  J = 0
  CALL HGRAM(SV(P), SV(Q), QPS, APP, AQQ, AQPR, AQPI, J)
  IF (J .LE. -HUGE(J)) THEN
     IF (E .NE. INPUT_UNIT) WRITE (E,9) 'GRAM:', INT(AIMAG(TOL)), ',', P, ',', Q, ',', J
     INFO = -7
     RETURN
  END IF
  I = 0
  IF (L .EQ. 0) THEN
     CALL HLJTU2(APP, AQQ, AQPR, AQPI, C, TR, TI, I)
     IF (I .GT. 0) THEN
        J = 0
        CALL HRTT(N, V(1,IX(P)), V(1,IX(Q)), C, TR, TI, GX, J)
        IF (J .LT. 0) THEN
           IF (E .NE. INPUT_UNIT) WRITE (E,9) 'RTT(V):', INT(AIMAG(TOL)), ',', P, ',', Q, ',', J
           INFO = -5
           RETURN
        END IF
        J = 1
        CALL HRTT(M, G(1,IX(P)), G(1,IX(Q)), C, TR, TI, GX, J)
        IF (J .LT. 0) THEN
           IF (E .NE. INPUT_UNIT) WRITE (E,9) 'RTT(G):', INT(AIMAG(TOL)), ',', P, ',', Q, ',', J
           INFO = -13
           RETURN
        END IF
     END IF
  ELSE ! hyp
     C = CUTOFF
     CALL HLJTV2(APP, AQQ, AQPR, AQPI, C, TR, TI, I)
     IF (I .GT. 0) THEN
        J = 0
        CALL HRTH(N, V(1,IX(P)), V(1,IX(Q)), C, TR, TI, GX, J)
        IF (J .LT. 0) THEN
           IF (E .NE. INPUT_UNIT) WRITE (E,9) 'RTH(V):', INT(AIMAG(TOL)), ',', P, ',', Q, ',', J
           INFO = -5
           RETURN
        END IF
        J = 1
        CALL HRTH(M, G(1,IX(P)), G(1,IX(Q)), C, TR, TI, GX, J)
        IF (J .LT. 0) THEN
           IF (E .NE. INPUT_UNIT) WRITE (E,9) 'RTH(G):', INT(AIMAG(TOL)), ',', P, ',', Q, ',', J
           INFO = -13
           RETURN
        END IF
     END IF
  END IF
  TOL = CMPLX(AQPR, AQPI, K)
  IF (I .EQ. 0) THEN
     INFO = 1
  ELSE IF (I .LT. 0) THEN
     IF (E .NE. INPUT_UNIT) THEN
        IF (L .EQ. 0) THEN
           WRITE (E,9) 'LJTU2:', INT(AIMAG(TOL)), ',', P, ',', Q, ',', I
        ELSE ! hyp
           WRITE (E,9) 'LJTV2:', INT(AIMAG(TOL)), ',', P, ',', Q, ',', I
        END IF
     END IF
     INFO = -8
  ELSE ! I > 0
     IF (O .EQ. 0) THEN
        ! S = ABS(A21 / (SV(P) * SV(Q)))
        ! norm update, trig:
        ! SQRT(SV(P) + TG * (S * SV(Q))) * SQRT(SV(P))
        ! SQRT(SV(Q) - TG * (S * SV(P))) * SQRT(SV(Q))
        ! norm update, hyp:
        ! SQRT(SV(P) + TH * (S * SV(Q))) * SQRT(SV(P))
        ! SQRT(SV(Q) + TH * (S * SV(P))) * SQRT(SV(Q))
        APP = S * SV(Q)
        AQQ = S * SV(P)
        IF (L .EQ. 0) THEN
           AQPI = -AQPR
        ELSE ! hyp
           AQPI =  AQPR
        END IF
        APP = GSQRT(GFMA(AQPR, APP, SV(P)))
        AQQ = GSQRT(GFMA(AQPI, AQQ, SV(Q)))
        SV(P) = APP * GSQRT(SV(P))
        SV(Q) = AQQ * GSQRT(SV(Q))
     ELSE ! SLOW
        SV(P) = HNRMF(M, G(1,IX(P)))
        SV(Q) = HNRMF(M, G(1,IX(Q)))
     END IF
     INFO = I + 1
  END IF
8 WRK(P,Q) = CMPLX(C, S, K)
  WRK(Q,P) = CMPLX(TR, TI, K)
9 FORMAT(4(A,I11))

!  IN: INFO = 0 or 1 (sin => tan) OR 2 (the modified deRijk)
! OUT: INFO: #sweeps
SUBROUTINE MJEVDR(N, A, LDA, V, LDV, JPOS, WRK, INFO)
  USE MPFR_F
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE MSWPXD(N, A, LDA, V, LDV, B, E, INFO)
       USE MPFR_F
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, B, E
       TYPE(MPFR_T), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE MSWPXD
  END INTERFACE
  INTERFACE
     SUBROUTINE MTRANA(N, A, LDA, V, LDV, P, Q, TOL, INFO)
       USE MPFR_F
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, P, Q
       TYPE(MPFR_T), INTENT(INOUT) :: A(LDA,N), V(LDV,N), TOL
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE MTRANA
  END INTERFACE
  INTERFACE
     SUBROUTINE MTRACE(N, A, LDA, SWP, NTR)
       USE MPFR_F
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, SWP, NTR
       TYPE(MPFR_T), INTENT(IN) :: A(LDA,N)
     END SUBROUTINE MTRACE
  END INTERFACE
  REAL(KIND=REAL64), PARAMETER :: ONE = 1.0_REAL64, EPS = EPSILON(EPS) / 2
  INTEGER, INTENT(IN) :: N, LDA, LDV, JPOS
  TYPE(MPFR_T), INTENT(INOUT) :: A(LDA,N)
  TYPE(MPFR_T), INTENT(OUT) :: V(LDV,N), WRK(N,N)
  INTEGER, INTENT(INOUT) :: INFO
  TYPE(MPFR_T) :: TOL
#ifdef __GFORTRAN__
  REAL(KIND=c_long_double) :: AX
#else
  REAL(KIND=REAL128) :: AX
#endif
  INTEGER(KIND=INT64) :: TT
  INTEGER :: O, P, Q, R, S, T, W, X
  CHARACTER(LEN=11) :: FN
  IF ((INFO .LT. 0) .OR. (INFO .GT. 7)) INFO = -8
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -6
  IF (LDV .LT. N) INFO = -5
  IF (LDA .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) RETURN
  CALL MPFR_INIT_M(TOL)
  S = HUGE(S)
  ! prescale G
  R = 0
  ! init V
  DO Q = 1, N
     DO P = 1, Q-1
        CALL MPFR_SET_ZERO(V(P,Q), V(P,Q)%TAG)
     END DO
     V(Q,Q) = ONE
     DO P = Q+1, N
        CALL MPFR_SET_ZERO(V(P,Q), V(P,Q)%TAG)
     END DO
  END DO
  ! init WRK
  DO Q = 1, N
     DO P = 1, N
        CALL MPFR_SET_ZERO(WRK(P,Q), WRK(P,Q)%TAG)
     END DO
  END DO
  ! init TOL
  TOL = SQRT(REAL(N, REAL64)) * EPS
  ! init trace
  R = 0
  O = ICHAR('d')
  CALL MTRACE(N, A, LDA, R, S)
  TT = 0_INT64
  ! main loop
  DO R = 1, S
     T = 0
     IF (IAND(INFO, 2) .EQ. 0) THEN
        DO P = 1, N-1
           IF (P .LT. JPOS) THEN
              CALL MSWPXD(N, A, LDA, V, LDV, P, JPOS, W)
           ELSE IF (P .GT. JPOS) THEN
              CALL MSWPXD(N, A, LDA, V, LDV, P, N, W)
           ELSE ! P = JPOS
              W = JPOS
           END IF
           IF (W .LE. 0) THEN
              INFO = -7
              RETURN
           END IF
           DO Q = P+1, N
              W = IAND(INFO, 1)
              IF ((P .LE. JPOS) .AND. (Q .GT. JPOS)) W = IOR(W, 2)
              CALL MPFR_SET_M(WRK(Q,P), TOL)
              CALL MTRANA(N, A, LDA, V, LDV, P, Q, WRK(Q,P), W)
              SELECT CASE (W)
              CASE (0)
                 CONTINUE
              CASE (1)
                 T = T + 1
              CASE (2,3)
                 T = T + 1
                 TT = TT + 1_INT64
              CASE DEFAULT
                 INFO = -4
                 RETURN
              END SELECT
           END DO
        END DO
     ELSE ! the modified deRijk
        ! the first diagonal block
        DO P = 1, JPOS-1
           CALL MSWPXD(N, A, LDA, V, LDV, P, JPOS, W)
           IF (W .LE. 0) THEN
              INFO = -7
              RETURN
           END IF
           DO Q = P+1, JPOS
              W = IAND(INFO, 1)
              CALL MPFR_SET_M(WRK(Q,P), TOL)
              CALL MTRANA(N, A, LDA, V, LDV, P, Q, WRK(Q,P), W)
              SELECT CASE (W)
              CASE (0)
                 CONTINUE
              CASE (1)
                 T = T + 1
              CASE (2,3)
                 T = T + 1
                 TT = TT + 1_INT64
              CASE DEFAULT
                 INFO = -4
                 RETURN
              END SELECT
           END DO
        END DO
        ! the off-diagonal block (hyp)
        DO P = 1, JPOS
           DO Q = JPOS+1, N
              W = IOR(IAND(INFO, 1), 2)
              CALL MPFR_SET_M(WRK(Q,P), TOL)
              CALL MTRANA(N, A, LDA, V, LDV, P, Q, WRK(Q,P), W)
              SELECT CASE (W)
              CASE (0)
                 CONTINUE
              CASE (1)
                 T = T + 1
              CASE (2,3)
                 T = T + 1
                 TT = TT + 1_INT64
              CASE DEFAULT
                 INFO = -4
                 RETURN
              END SELECT
           END DO
        END DO
        ! the second diagonal block
        DO P = JPOS+1, N-1
           CALL MSWPXD(N, A, LDA, V, LDV, P, N, W)
           IF (W .LE. 0) THEN
              INFO = -7
              RETURN
           END IF
           DO Q = P+1, N
              W = IAND(INFO, 1)
              CALL MPFR_SET_M(WRK(Q,P), TOL)
              CALL MTRANA(N, A, LDA, V, LDV, P, Q, WRK(Q,P), W)
              SELECT CASE (W)
              CASE (0)
                 CONTINUE
              CASE (1)
                 T = T + 1
              CASE (2,3)
                 T = T + 1
                 TT = TT + 1_INT64
              CASE DEFAULT
                 INFO = -5
                 RETURN
              END SELECT
           END DO
        END DO
     END IF
     CALL MTRACE(N, A, LDA, R, T)
     IF (N .LT. 1000) THEN
        DO Q = 1, N
           DO P = 1, Q-1
              CALL MPFR_SET_M(WRK(P,Q), A(Q,P))
           END DO
           CALL MPFR_SET_M(WRK(Q,Q),A(Q,Q))
        END DO
        IF (IAND(INFO, 2) .EQ. 0) THEN
           WRITE (FN,'(A,I3.3,A,I2.2,A)') 'd', N, '_', R, '.txt'
        ELSE ! the modified deRijk
           WRITE (FN,'(A,I3.3,A,I2.2,A)') 'd', N, '-', R, '.txt'
        END IF
        OPEN(NEWUNIT=W, IOSTAT=X, FILE=FN, STATUS='REPLACE', ACTION='WRITE', ACCESS='SEQUENTIAL', FORM='FORMATTED')
        IF (N .EQ. 1) THEN
           AX = WRK(1,1)
           WRITE (W,8) AX
        ELSE ! N > 1
           DO P = 1, N
              DO Q = 1, N-1
                 IF (Q .EQ. 1) THEN
                    AX = WRK(P,1)
                    WRITE (W,8,ADVANCE='NO') AX
                 ELSE ! Q > 1
                    AX = WRK(P,Q)
                    WRITE (W,9,ADVANCE='NO') AX
                 END IF
              END DO
              AX = WRK(P,N)
              WRITE (W,9) AX
           END DO
        END IF
        CLOSE(UNIT=W, IOSTAT=X)
     END IF
     IF (T .EQ. 0) EXIT
  END DO
  INFO = R
  WRK(1,1) = TT
  CALL MPFR_CLEAR_M(TOL)
#ifdef __GFORTRAN__
8 FORMAT(ES30.21E4)
9 FORMAT(ES31.21E4)
#else
8 FORMAT(ES45.36E4)
9 FORMAT(ES46.36E4)
#endif
END SUBROUTINE MJEVDR

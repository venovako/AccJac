!  IN: AS = max sweeps, INFO = 0 or 1 (sin => tan) OR 2 (the modified deRijk)
! OUT: AS: backscale A by 2**-AS, INFO: #sweeps
SUBROUTINE SJEVDR(N, A, LDA, V, LDV, JPOS, WRK, AS, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL32
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE SDSORT(N, A, LDA, V, LDV, JPOS, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, JPOS
       REAL(KIND=REAL32), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE SDSORT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE SSCALA(N, A, LDA, AX, AS, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA
       REAL(KIND=REAL32), INTENT(INOUT) :: A(LDA,N), AX
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE SSCALA
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE SSWPXD(N, A, LDA, V, LDV, B, E, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, B, E
       REAL(KIND=REAL32), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE SSWPXD
  END INTERFACE
  INTERFACE
     SUBROUTINE STRANA(N, A, LDA, V, LDV, AX, AS, P, Q, TOL, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, P, Q
       REAL(KIND=REAL32), INTENT(INOUT) :: A(LDA,N), V(LDV,N), AX, TOL
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE STRANA
  END INTERFACE
  INTERFACE
     SUBROUTINE STRACE(N, A, LDA, AX, AS, SWP, NTR)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, AS, SWP, NTR
       REAL(KIND=REAL32), INTENT(IN) :: A(LDA,N), AX
     END SUBROUTINE STRACE
  END INTERFACE
  INTERFACE
     SUBROUTINE STRCOA(N, A, LDA, AS, S, T, U)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, AS, S
       REAL(KIND=REAL32), INTENT(IN) :: A(LDA,N)
       INTEGER, INTENT(INOUT) :: T, U
     END SUBROUTINE STRCOA
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL32
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, EPS = EPSILON(EPS) / 2
  INTEGER, INTENT(IN) :: N, LDA, LDV, JPOS
  REAL(KIND=K), INTENT(INOUT) :: A(LDA,N), WRK(N,N)
  REAL(KIND=K), INTENT(OUT) :: V(LDV,N)
  INTEGER, INTENT(INOUT) :: AS, INFO
  REAL(KIND=K) :: AX, TOL
  INTEGER(KIND=INT64) :: TT
  INTEGER :: L, O, P, Q, R, S, T, U, W, X
  CHARACTER(LEN=11) :: FN
  IF ((INFO .LT. 0) .OR. (INFO .GT. 7)) INFO = -9
  IF (AS .LT. 0) INFO = -8
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -6
  IF (LDV .LT. N) INFO = -5
  IF (LDA .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) RETURN
  L = INT(WRK(1,1))
  S = AS
  ! prescale G
  AX = ZERO
  AS = 0
  R = 0
  CALL SSCALA(N, A, LDA, AX, AS, R)
  IF (R .LT. 0) THEN
     INFO = -2
     RETURN
  END IF
  ! init V
  DO Q = 1, N
     DO P = 1, Q-1
        V(P,Q) = ZERO
     END DO
     V(Q,Q) = ONE
     DO P = Q+1, N
        V(P,Q) = ZERO
     END DO
  END DO
  ! init TOL
  TOL = N
  TOL = SQRT(TOL) * EPS
  ! init trace
  R = 0
  O = ICHAR('s')
  CALL STRACE(N, A, LDA, AX, AS, R, S)
  IF (IAND(INFO, 4) .NE. 0) CALL STRCOA(N, A, LDA, AS, R, O, U)
  TT = 0_INT64
  ! main loop
  DO R = 1, S
     T = 0
     W = 0
     IF (L .NE. 0) CALL SDSORT(N, A, LDA, V, LDV, JPOS, W)
     IF (W .LT. 0) THEN
        INFO = -7
        RETURN
     END IF
     IF (IAND(INFO, 2) .EQ. 0) THEN
        DO P = 1, N-1
           IF (P .LT. JPOS) THEN
              CALL SSWPXD(N, A, LDA, V, LDV, P, JPOS, W)
           ELSE IF (P .GT. JPOS) THEN
              CALL SSWPXD(N, A, LDA, V, LDV, P, N, W)
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
              WRK(Q,P) = TOL
              CALL STRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(Q,P), W)
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
              IF (IAND(INFO, 4) .NE. 0) CALL STRCOA(N, A, LDA, AS, R, O, U)
           END DO
        END DO
     ELSE ! the modified deRijk
        ! the first diagonal block
        DO P = 1, JPOS-1
           CALL SSWPXD(N, A, LDA, V, LDV, P, JPOS, W)
           IF (W .LE. 0) THEN
              INFO = -7
              RETURN
           END IF
           DO Q = P+1, JPOS
              W = IAND(INFO, 1)
              WRK(Q,P) = TOL
              CALL STRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(Q,P), W)
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
              IF (IAND(INFO, 4) .NE. 0) CALL STRCOA(N, A, LDA, AS, R, O, U)
           END DO
        END DO
        ! the off-diagonal block (hyp)
        DO P = 1, JPOS
           DO Q = JPOS+1, N
              W = IOR(IAND(INFO, 1), 2)
              WRK(Q,P) = TOL
              CALL STRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(Q,P), W)
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
              IF (IAND(INFO, 4) .NE. 0) CALL STRCOA(N, A, LDA, AS, R, O, U)
           END DO
        END DO
        ! the second diagonal block
        DO P = JPOS+1, N-1
           CALL SSWPXD(N, A, LDA, V, LDV, P, N, W)
           IF (W .LE. 0) THEN
              INFO = -7
              RETURN
           END IF
           DO Q = P+1, N
              W = IAND(INFO, 1)
              WRK(Q,P) = TOL
              CALL STRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(Q,P), W)
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
              IF (IAND(INFO, 4) .NE. 0) CALL STRCOA(N, A, LDA, AS, R, O, U)
           END DO
        END DO
     END IF
     CALL STRACE(N, A, LDA, AX, AS, R, T)
     IF (N .LT. 1000) THEN
        X = -AS
        DO Q = 1, N
           DO P = 1, Q-1
              WRK(P,Q) = SCALE(A(Q,P), X)
           END DO
           WRK(Q,Q) = SCALE(A(Q,Q), X)
        END DO
        IF (IAND(INFO, 2) .EQ. 0) THEN
           WRITE (FN,'(A,I3.3,A,I2.2,A)') 's', N, '_', R, '.txt'
        ELSE ! the modified deRijk
           WRITE (FN,'(A,I3.3,A,I2.2,A)') 's', N, '-', R, '.txt'
        END IF
        OPEN(NEWUNIT=W, IOSTAT=X, FILE=FN, STATUS='REPLACE', ACTION='WRITE', ACCESS='SEQUENTIAL', FORM='FORMATTED')
        IF (N .EQ. 1) THEN
           WRITE (W,'(ES16.9E2)') WRK(1,1)
        ELSE ! N > 1
           DO P = 1, N
              DO Q = 1, N-1
                 IF (Q .EQ. 1) THEN
                    WRITE (W,'(ES16.9E2)',ADVANCE='NO') WRK(P,1)
                 ELSE ! Q > 1
                    WRITE (W,'(ES17.9E2)',ADVANCE='NO') WRK(P,Q)
                 END IF
              END DO
              WRITE (W,'(ES17.9E2)') WRK(P,N)
           END DO
        END IF
        CLOSE(UNIT=W, IOSTAT=X)
     END IF
     IF (T .EQ. 0) EXIT
  END DO
  IF (IAND(INFO, 4) .NE. 0) CALL STRCOA(N, A, LDA, AS, -1, O, U)
  INFO = R
  WRK(1,1) = REAL(TT, K)
END SUBROUTINE SJEVDR

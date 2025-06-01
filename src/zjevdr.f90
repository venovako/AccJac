!  IN: AS = max sweeps, INFO = 0 or 1 (sin => tan) OR 2 (the modified deRijk)
! OUT: AS: backscale A by 2**-AS, INFO: #sweeps
SUBROUTINE ZJEVDR(N, A, LDA, V, LDV, JPOS, WRK, AS, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL64
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE ZDSORT(N, A, LDA, V, LDV, JPOS, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, JPOS
       COMPLEX(KIND=REAL64), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE ZDSORT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE ZSCALA(N, A, LDA, AX, AS, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA
       COMPLEX(KIND=REAL64), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=REAL64), INTENT(INOUT) :: AX
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE ZSCALA
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE ZSWPXD(N, A, LDA, V, LDV, B, E, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, B, E
       COMPLEX(KIND=REAL64), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE ZSWPXD
  END INTERFACE
  INTERFACE
     SUBROUTINE ZTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, TOL, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, P, Q
       COMPLEX(KIND=REAL64), INTENT(INOUT) :: A(LDA,N), V(LDV,N), TOL
       REAL(KIND=REAL64), INTENT(INOUT) :: AX
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE ZTRANA
  END INTERFACE
  INTERFACE
     SUBROUTINE ZTRACE(N, A, LDA, AX, AS, SWP, NTR)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, AS, SWP, NTR
       COMPLEX(KIND=REAL64), INTENT(IN) :: A(LDA,N)
       REAL(KIND=REAL64), INTENT(IN) :: AX
     END SUBROUTINE ZTRACE
  END INTERFACE
  INTERFACE
     SUBROUTINE ZTRCOA(N, A, LDA, AS, S, T, U)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, AS, S
       COMPLEX(KIND=REAL64), INTENT(IN) :: A(LDA,N)
       INTEGER, INTENT(INOUT) :: T, U
     END SUBROUTINE ZTRCOA
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, EPS = EPSILON(EPS) / 2
  INTEGER, INTENT(IN) :: N, LDA, LDV, JPOS
  COMPLEX(KIND=K), INTENT(INOUT) :: A(LDA,N), WRK(N,N)
  COMPLEX(KIND=K), INTENT(OUT) :: V(LDV,N)
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
  L = INT(REAL(WRK(1,1)))
  S = AS
  ! prescale G
  AX = ZERO
  AS = 0
  R = 0
  CALL ZSCALA(N, A, LDA, AX, AS, R)
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
  O = ICHAR('z')
  CALL ZTRACE(N, A, LDA, AX, AS, R, S)
  IF (IAND(INFO, 4) .NE. 0) CALL ZTRCOA(N, A, LDA, AS, R, O, U)
  TT = 0_INT64
  ! main loop
  DO R = 1, S
     T = 0
     W = 0
     IF (L .NE. 0) CALL ZDSORT(N, A, LDA, V, LDV, JPOS, W)
     IF (W .LT. 0) THEN
        INFO = -7
        RETURN
     END IF
     IF (IAND(INFO, 2) .EQ. 0) THEN
        DO P = 1, N-1
           IF (P .LT. JPOS) THEN
              CALL ZSWPXD(N, A, LDA, V, LDV, P, JPOS, W)
           ELSE IF (P .GT. JPOS) THEN
              CALL ZSWPXD(N, A, LDA, V, LDV, P, N, W)
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
              CALL ZTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(Q,P), W)
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
              IF (IAND(INFO, 4) .NE. 0) CALL ZTRCOA(N, A, LDA, AS, R, O, U)
           END DO
        END DO
     ELSE ! the modified deRijk
        ! the first diagonal block
        DO P = 1, JPOS-1
           CALL ZSWPXD(N, A, LDA, V, LDV, P, JPOS, W)
           IF (W .LE. 0) THEN
              INFO = -7
              RETURN
           END IF
           DO Q = P+1, JPOS
              W = IAND(INFO, 1)
              WRK(Q,P) = TOL
              CALL ZTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(Q,P), W)
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
              IF (IAND(INFO, 4) .NE. 0) CALL ZTRCOA(N, A, LDA, AS, R, O, U)
           END DO
        END DO
        ! the off-diagonal block (hyp)
        DO P = 1, JPOS
           DO Q = JPOS+1, N
              W = IOR(IAND(INFO, 1), 2)
              WRK(Q,P) = TOL
              CALL ZTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(Q,P), W)
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
              IF (IAND(INFO, 4) .NE. 0) CALL ZTRCOA(N, A, LDA, AS, R, O, U)
           END DO
        END DO
        ! the second diagonal block
        DO P = JPOS+1, N-1
           CALL ZSWPXD(N, A, LDA, V, LDV, P, N, W)
           IF (W .LE. 0) THEN
              INFO = -7
              RETURN
           END IF
           DO Q = P+1, N
              W = IAND(INFO, 1)
              WRK(Q,P) = TOL
              CALL ZTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(Q,P), W)
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
              IF (IAND(INFO, 4) .NE. 0) CALL ZTRCOA(N, A, LDA, AS, R, O, U)
           END DO
        END DO
     END IF
     CALL ZTRACE(N, A, LDA, AX, AS, R, T)
     IF (N .LT. 1000) THEN
        X = -AS
        DO Q = 1, N
           DO P = 1, Q-1
              WRK(P,Q) = CMPLX(SCALE(REAL(A(Q,P)), X), SCALE(-AIMAG(A(Q,P)), X), K)
           END DO
           WRK(Q,Q) = CMPLX(SCALE(REAL(A(Q,Q)), X), ZERO, K)
        END DO
        IF (IAND(INFO, 2) .EQ. 0) THEN
           WRITE (FN,'(A,I3.3,A,I2.2,A)') 'z', N, '_', R, '.txt'
        ELSE ! the modified deRijk
           WRITE (FN,'(A,I3.3,A,I2.2,A)') 'z', N, '-', R, '.txt'
        END IF
        OPEN(NEWUNIT=W, IOSTAT=X, FILE=FN, STATUS='REPLACE', ACTION='WRITE', ACCESS='SEQUENTIAL', FORM='FORMATTED')
        IF (N .EQ. 1) THEN
           WRITE (W,'(2(A,ES25.17E3),A)') '(', REAL(WRK(1,1)), ',', AIMAG(WRK(1,1)), ')'
        ELSE ! N > 1
           DO P = 1, N
              DO Q = 1, N-1
                 IF (Q .EQ. 1) THEN
                    WRITE (W,'(2(A,ES25.17E3),A)',ADVANCE='NO') '(', REAL(WRK(P,1)), ',', AIMAG(WRK(P,1)), ')'
                 ELSE ! Q > 1
                    WRITE (W,'(2(A,ES25.17E3),A)',ADVANCE='NO') ' (', REAL(WRK(P,Q)), ',', AIMAG(WRK(P,Q)), ')'
                 END IF
              END DO
              WRITE (W,'(2(A,ES25.17E3),A)') ' (', REAL(WRK(P,N)), ',', AIMAG(WRK(P,N)), ')'
           END DO
        END IF
        CLOSE(UNIT=W, IOSTAT=X)
     END IF
     IF (T .EQ. 0) EXIT
  END DO
  IF (IAND(INFO, 4) .NE. 0) CALL ZTRCOA(N, A, LDA, AS, -1, O, U)
  INFO = R
  WRK(1,1) = TT
END SUBROUTINE ZJEVDR

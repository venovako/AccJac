!  IN: AS = max sweeps, INFO = 0 or 1 (sin => tan)
! OUT: AS: backscale A by 2**-AS, INFO: #sweeps
SUBROUTINE DJEVDR(N, A, LDA, V, LDV, JPOS, WRK, AS, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL64
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE DSCALA(N, A, LDA, AX, AS, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA
       REAL(KIND=REAL64), INTENT(INOUT) :: A(LDA,N), AX
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE DSCALA
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE DSWPXD(N, A, LDA, V, LDV, B, E, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, B, E
       REAL(KIND=REAL64), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE DSWPXD
  END INTERFACE
  INTERFACE
     SUBROUTINE DTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, TOL, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, P, Q
       REAL(KIND=REAL64), INTENT(INOUT) :: A(LDA,N), V(LDV,N), AX, TOL
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE DTRANA
  END INTERFACE
  INTERFACE
     SUBROUTINE DTRACE(N, A, LDA, AX, AS, SWP, NTR)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, AS, SWP, NTR
       REAL(KIND=REAL64), INTENT(IN) :: A(LDA,N), AX
     END SUBROUTINE DTRACE
  END INTERFACE
  INTERFACE
     SUBROUTINE DTRCOA(N, A, LDA, AS, S, T, U)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, AS, S
       REAL(KIND=REAL64), INTENT(IN) :: A(LDA,N)
       INTEGER, INTENT(INOUT) :: T, U
     END SUBROUTINE DTRCOA
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, EPS = EPSILON(EPS) / 2
  INTEGER, INTENT(IN) :: N, LDA, LDV, JPOS
  REAL(KIND=K), INTENT(INOUT) :: A(LDA,N)
  REAL(KIND=K), INTENT(OUT) :: V(LDV,N), WRK(N,N)
  INTEGER, INTENT(INOUT) :: AS, INFO
  REAL(KIND=K) :: AX, TOL
  INTEGER(KIND=INT64) :: TT
  INTEGER :: O, P, Q, R, S, T, U, W
  IF ((INFO .LT. 0) .OR. (INFO .GT. 7)) INFO = -9
  IF (AS .LT. 0) INFO = -8
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -6
  IF (LDV .LT. N) INFO = -5
  IF (LDA .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) RETURN
  S = AS
  ! prescale G
  AX = ZERO
  AS = 0
  R = 0
  CALL DSCALA(N, A, LDA, AX, AS, R)
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
  ! init WRK
  WRK = ZERO
  ! init TOL
  TOL = N
  TOL = SQRT(TOL) * EPS
  ! init trace
  R = 0
  CALL DTRACE(N, A, LDA, AX, AS, R, S)
  IF (IAND(INFO, 4) .NE. 0) CALL DTRCOA(N, A, LDA, AS, R, O, U)
  TT = 0_INT64
  ! main loop
  DO R = 1, S
     T = 0
     ! the first diagonal block
     DO P = 1, JPOS-1
        CALL DSWPXD(N, A, LDA, V, LDV, P, JPOS, W)
        IF (W .LE. 0) THEN
           INFO = -7
           RETURN
        END IF
        DO Q = P+1, JPOS
           W = IAND(INFO, 1)
           WRK(P,Q) = TOL
           CALL DTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(P,Q), W)
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
           IF (IAND(INFO, 4) .NE. 0) CALL DTRCOA(N, A, LDA, AS, R, O, U)
        END DO
     END DO
     ! the off-diagonal block (hyp)
     DO P = 1, JPOS
        DO Q = JPOS+1, N
           W = IOR(IAND(INFO, 1), 2)
           WRK(P,Q) = TOL
           CALL DTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(P,Q), W)
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
           IF (IAND(INFO, 4) .NE. 0) CALL DTRCOA(N, A, LDA, AS, R, O, U)
        END DO
     END DO
     ! the second diagonal block
     DO P = JPOS+1, N-1
        CALL DSWPXD(N, A, LDA, V, LDV, P, N, W)
        IF (W .LE. 0) THEN
           INFO = -7
           RETURN
        END IF
        DO Q = P+1, N
           W = IAND(INFO, 1)
           WRK(P,Q) = TOL
           CALL DTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(P,Q), W)
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
           IF (IAND(INFO, 4) .NE. 0) CALL DTRCOA(N, A, LDA, AS, R, O, U)
        END DO
     END DO
     CALL DTRACE(N, A, LDA, AX, AS, R, T)
     IF (T .EQ. 0) EXIT
  END DO
  IF (IAND(INFO, 4) .NE. 0) CALL DTRCOA(N, A, LDA, AS, -1, O, U)
  INFO = R
  WRK(1,1) = TRANSFER(TT, ZERO)
END SUBROUTINE DJEVDR

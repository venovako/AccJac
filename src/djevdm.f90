!  IN: AS = max sweeps, INFO = 0 or 1 (sin => tan) OR 2 (the modified modulus)
! OUT: AS: backscale A by 2**-AS, INFO: #sweeps
SUBROUTINE DJEVDM(N, A, LDA, V, LDV, JPOS, WRK, AS, TBL, ORD, INFO)
#ifdef ANIMATE
  USE, INTRINSIC :: ISO_C_BINDING
#endif
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL64
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE JSTEP(J, N, S, T, P, O, R, INFO)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: J, N, S, T, P, O(2,*)
       INTEGER, INTENT(OUT) :: R(2,P), INFO
     END SUBROUTINE JSTEP
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE DDSORT(N, A, LDA, V, LDV, JPOS, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, JPOS
       REAL(KIND=REAL64), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE DDSORT
  END INTERFACE
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
  INTEGER, INTENT(IN) :: N, LDA, LDV, JPOS, TBL(2,*)
  REAL(KIND=K), INTENT(INOUT) :: A(LDA,N), WRK(N,N)
  REAL(KIND=K), INTENT(OUT) :: V(LDV,N)
  INTEGER, INTENT(INOUT) :: AS, INFO
  INTEGER, INTENT(OUT) :: ORD(2,*)
  REAL(KIND=K) :: AX, TOL
  INTEGER(KIND=INT64) :: TT
  INTEGER :: L, O, P, Q, R, S, T, U, W, X, JJ, JS, ST
  CHARACTER(LEN=11) :: FN
#ifdef ANIMATE
  CHARACTER(LEN=256), POINTER :: CP
  INTEGER(KIND=c_size_t) :: LDW
  TYPE(c_ptr) :: CTX
  TYPE(c_ptr), EXTERNAL :: PVN_RVIS_START
  INTEGER(KIND=c_int), EXTERNAL :: PVN_RVIS_FRAME, PVN_RVIS_STOP
  CP => NULL()
#endif
  IF ((INFO .LT. 0) .OR. (INFO .GT. 7)) INFO = -11
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
  ! init TOL
  TOL = N
  TOL = SQRT(TOL) * EPS
  ! init trace
  R = 0
  O = ICHAR('d')
#ifdef NDEBUG
  CALL DTRACE(N, A, LDA, AX, AS, R, -S)
#else
  CALL DTRACE(N, A, LDA, AX, AS, R, S)
#endif
  IF (IAND(INFO, 4) .NE. 0) CALL DTRCOA(N, A, LDA, AS, R, O, U)
  TT = 0_INT64
  IF (IAND(INFO, 2) .EQ. 0) THEN
     JJ = 5
     JS = N - 1
  ELSE ! modified modulus
     JJ = 7
     JS = N
  END IF
#ifdef ANIMATE
  CTX = TRANSFER(WRK(1,2), CTX)
  CALL C_F_POINTER(CTX, CP)
  FN = TRIM(CP(1:8))//'lg'//c_null_char
  LDW = TRANSFER(WRK(2,1), LDW)
  W = INT(LDW)
  CTX = PVN_RVIS_START(N, N, W, FN)
  IF (.NOT. C_ASSOCIATED(CTX)) STOP 'PVN_RVIS_START'
  LDW = TRANSFER(WRK(2,2), LDW)
#endif
  ! main loop
  DO R = 1, S
     T = 0
     W = 0
     IF (L .NE. 0) CALL DDSORT(N, A, LDA, V, LDV, JPOS, W)
     IF (W .LT. 0) THEN
        INFO = -7
        RETURN
     END IF
#ifdef ANIMATE
     X = -AS
     IF (X .NE. 0) THEN
        DO Q = 1, N
           DO P = 1, Q-1
              WRK(P,Q) = SCALE(A(Q,P), X)
           END DO
           DO P = Q, N
              WRK(P,Q) = SCALE(A(P,Q), X)
           END DO
        END DO
     ELSE ! X = 0
        DO Q = 1, N
           DO P = 1, Q-1
              WRK(P,Q) = A(Q,P)
           END DO
           DO P = Q, N
              WRK(P,Q) = A(P,Q)
           END DO
        END DO
     END IF
     IF (PVN_RVIS_FRAME(CTX, WRK, LDW) .NE. 0_c_int) STOP 'PVN_RVIS_FRAME'
#endif
     P = N / 2
     DO ST = 1, JS
        CALL JSTEP(JJ, N, JS, ST, P, TBL, ORD, W)
        IF (W .NE. 0) THEN
           INFO = -9
           RETURN
        END IF
        ! TODO: in parallel
        DO Q = 1, P
           W = IAND(INFO, 1)
           WRK(ORD(2,Q),ORD(1,Q)) = TOL
           IF ((ORD(1,Q) .LE. JPOS) .AND. (ORD(2,Q) .GT. JPOS)) W = IOR(W, 2)
           CALL DTRANA(N, A, LDA, V, LDV, AX, AS, ORD(1,Q), ORD(2,Q), WRK(ORD(2,Q),ORD(1,Q)), W)
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
#ifdef NDEBUG
     CALL DTRACE(N, A, LDA, AX, AS, R, -T)
#else
     CALL DTRACE(N, A, LDA, AX, AS, R, T)
#endif
     IF (N .LT. 1000) THEN
        X = -AS
        DO Q = 1, N
           DO P = 1, Q-1
              WRK(P,Q) = SCALE(A(Q,P), X)
           END DO
           WRK(Q,Q) = SCALE(A(Q,Q), X)
        END DO
        IF (IAND(INFO, 2) .EQ. 0) THEN
           WRITE (FN,'(A,I3.3,A,I2.2,A)') 'd', N, '_', R, '.txt'
        ELSE ! modified modulus
           WRITE (FN,'(A,I3.3,A,I2.2,A)') 'd', N, '-', R, '.txt'
        END IF
        OPEN(NEWUNIT=W, IOSTAT=X, FILE=FN, STATUS='REPLACE', ACTION='WRITE', ACCESS='SEQUENTIAL', FORM='FORMATTED')
        IF (N .EQ. 1) THEN
           WRITE (W,'(ES25.17E3)') WRK(1,1)
        ELSE ! N > 1
           DO P = 1, N
              DO Q = 1, N-1
                 IF (Q .EQ. 1) THEN
                    WRITE (W,'(ES25.17E3)',ADVANCE='NO') WRK(P,1)
                 ELSE ! Q > 1
                    WRITE (W,'(ES26.17E3)',ADVANCE='NO') WRK(P,Q)
                 END IF
              END DO
              WRITE (W,'(ES26.17E3)') WRK(P,N)
           END DO
        END IF
        CLOSE(UNIT=W, IOSTAT=X)
     END IF
     IF (T .EQ. 0) THEN
#ifdef ANIMATE
        X = -AS
        IF (X .NE. 0) THEN
           DO Q = 1, N
              DO P = 1, Q-1
                 WRK(P,Q) = SCALE(A(Q,P), X)
              END DO
              DO P = Q, N
                 WRK(P,Q) = SCALE(A(P,Q), X)
              END DO
           END DO
        ELSE ! X = 0
           DO Q = 1, N
              DO P = 1, Q-1
                 WRK(P,Q) = A(Q,P)
              END DO
              DO P = Q, N
                 WRK(P,Q) = A(P,Q)
              END DO
           END DO
        END IF
        IF (PVN_RVIS_FRAME(CTX, WRK, LDW) .NE. 0_c_int) STOP 'PVN_RVIS_FRAME'
#endif
        EXIT
     END IF
  END DO
  IF (IAND(INFO, 4) .NE. 0) CALL DTRCOA(N, A, LDA, AS, -1, O, U)
#ifdef ANIMATE
  W = ANIMATE
  T = ANIMATE
  X = 8
  FN = TRIM(CP(1:8))//'lg'//c_null_char
  IF (PVN_RVIS_STOP(CTX, W, T, X, FN) .NE. 0_c_int) STOP 'PVN_RVIS_STOP'
#endif
  INFO = R
  WRK(1,1) = REAL(TT, K)
END SUBROUTINE DJEVDM

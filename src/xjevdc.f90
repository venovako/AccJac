!  IN: AS = max sweeps, INFO = 0 or 1 (sin => tan) OR 2 (column-cyclic)
! OUT: AS: backscale A by 2**-AS, INFO: #sweeps
SUBROUTINE XJEVDC(N, A, LDA, V, LDV, JPOS, WRK, AS, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL128
#endif
  IMPLICIT NONE
#include "cr.f90"
  INTERFACE
     PURE SUBROUTINE XDSORT(N, A, LDA, V, LDV, JPOS, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, JPOS
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
#endif
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE XDSORT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XSCALA(N, A, LDA, AX, AS, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N), AX
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A(LDA,N), AX
#endif
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE XSCALA
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XSWPXD(N, A, LDA, V, LDV, B, E, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, B, E
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
#endif
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE XSWPXD
  END INTERFACE
  INTERFACE
     SUBROUTINE XTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, TOL, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, P, Q
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N), V(LDV,N), AX, TOL
#else
       REAL(KIND=REAL128), INTENT(INOUT) :: A(LDA,N), V(LDV,N), AX, TOL
#endif
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE XTRANA
  END INTERFACE
  INTERFACE
     SUBROUTINE XTRACE(N, A, LDA, AX, AS, SWP, NTR)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, AS, SWP, NTR
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: A(LDA,N), AX
#else
       REAL(KIND=REAL128), INTENT(IN) :: A(LDA,N), AX
#endif
     END SUBROUTINE XTRACE
  END INTERFACE
  INTERFACE
     SUBROUTINE XTRCOA(N, A, LDA, AS, S, T, U)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, AS, S
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: A(LDA,N)
#else
       REAL(KIND=REAL128), INTENT(IN) :: A(LDA,N)
#endif
       INTEGER, INTENT(INOUT) :: T, U
     END SUBROUTINE XTRCOA
  END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
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
  CALL XSCALA(N, A, LDA, AX, AS, R)
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
  TOL = CR_SQRT(TOL) * EPS
  ! init trace
  R = 0
  O = ICHAR('X')
#ifdef NDEBUG
  CALL XTRACE(N, A, LDA, AX, AS, R, -S)
#else
  CALL XTRACE(N, A, LDA, AX, AS, R, S)
#endif
  IF (IAND(INFO, 4) .NE. 0) CALL XTRCOA(N, A, LDA, AS, R, O, U)
  TT = 0_INT64
  ! main loop
  DO R = 1, S
     T = 0
     W = 0
     IF (L .NE. 0) CALL XDSORT(N, A, LDA, V, LDV, JPOS, W)
     IF (W .LT. 0) THEN
        INFO = -7
        RETURN
     END IF
     IF (IAND(INFO, 2) .EQ. 0) THEN
        DO P = 1, N-1
           DO Q = P+1, N
              W = IAND(INFO, 1)
              IF ((P .LE. JPOS) .AND. (Q .GT. JPOS)) W = IOR(W, 2)
              WRK(Q,P) = TOL
              CALL XTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(Q,P), W)
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
              IF (IAND(INFO, 4) .NE. 0) CALL XTRCOA(N, A, LDA, AS, R, O, U)
           END DO
        END DO
     ELSE ! column-cyclic
        DO Q = 2, N
           DO P = 1, Q-1
              W = IAND(INFO, 1)
              IF ((P .LE. JPOS) .AND. (Q .GT. JPOS)) W = IOR(W, 2)
              WRK(Q,P) = TOL
              CALL XTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(Q,P), W)
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
              IF (IAND(INFO, 4) .NE. 0) CALL XTRCOA(N, A, LDA, AS, R, O, U)
           END DO
        END DO
     END IF
#ifdef NDEBUG
     CALL XTRACE(N, A, LDA, AX, AS, R, -T)
#else
     CALL XTRACE(N, A, LDA, AX, AS, R, T)
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
           WRITE (FN,'(A,I3.3,A,I2.2,A)') 'X', N, '_', R, '.txt'
        ELSE ! column-cyclic
           WRITE (FN,'(A,I3.3,A,I2.2,A)') 'X', N, '-', R, '.txt'
        END IF
        OPEN(NEWUNIT=W, IOSTAT=X, FILE=FN, STATUS='REPLACE', ACTION='WRITE', ACCESS='SEQUENTIAL', FORM='FORMATTED')
        IF (N .EQ. 1) THEN
           WRITE (W,8) WRK(1,1)
        ELSE ! N > 1
           DO P = 1, N
              DO Q = 1, N-1
                 IF (Q .EQ. 1) THEN
                    WRITE (W,8,ADVANCE='NO') WRK(P,1)
                 ELSE ! Q > 1
                    WRITE (W,9,ADVANCE='NO') WRK(P,Q)
                 END IF
              END DO
              WRITE (W,9) WRK(P,N)
           END DO
        END IF
        CLOSE(UNIT=W, IOSTAT=X)
     END IF
     IF (T .EQ. 0) EXIT
  END DO
  IF (IAND(INFO, 4) .NE. 0) CALL XTRCOA(N, A, LDA, AS, -1, O, U)
  INFO = R
  WRK(1,1) = REAL(TT, K)
#ifdef __GFORTRAN__
8 FORMAT(ES30.21E4)
9 FORMAT(ES31.21E4)
#else
8 FORMAT(ES45.36E4)
9 FORMAT(ES46.36E4)
#endif
END SUBROUTINE XJEVDC

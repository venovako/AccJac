!  IN: AS = max sweeps, INFO = 0 or 1 (sin => tan) OR 2 (the modified deRijk)
! OUT: AS: backscale A by 2**-AS, INFO: #sweeps
SUBROUTINE WJEVDC(N, A, LDA, V, LDV, JPOS, WRK, AS, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL128
#endif
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE WSCALA(N, A, LDA, AX, AS, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: AX
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: A(LDA,N)
       REAL(KIND=REAL128), INTENT(INOUT) :: AX
#endif
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE WSCALA
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WSWPXD(N, A, LDA, V, LDV, B, E, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, B, E
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: A(LDA,N), V(LDV,N)
#endif
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE WSWPXD
  END INTERFACE
  INTERFACE
     SUBROUTINE WTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, TOL, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDV, P, Q
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: A(LDA,N), V(LDV,N), TOL
       REAL(KIND=c_long_double), INTENT(INOUT) :: AX
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: A(LDA,N), V(LDV,N), TOL
       REAL(KIND=REAL128), INTENT(INOUT) :: AX
#endif
       INTEGER, INTENT(INOUT) :: AS, INFO
     END SUBROUTINE WTRANA
  END INTERFACE
  INTERFACE
     SUBROUTINE WTRACE(N, A, LDA, AX, AS, SWP, NTR)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, AS, SWP, NTR
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(IN) :: A(LDA,N)
       REAL(KIND=c_long_double), INTENT(IN) :: AX
#else
       COMPLEX(KIND=REAL128), INTENT(IN) :: A(LDA,N)
       REAL(KIND=REAL128), INTENT(IN) :: AX
#endif
     END SUBROUTINE WTRACE
  END INTERFACE
  INTERFACE
     SUBROUTINE WTRCOA(N, A, LDA, AS, S, T, U)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, AS, S
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(IN) :: A(LDA,N)
#else
       COMPLEX(KIND=REAL128), INTENT(IN) :: A(LDA,N)
#endif
       INTEGER, INTENT(INOUT) :: T, U
     END SUBROUTINE WTRCOA
  END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, EPS = EPSILON(EPS) / 2
  INTEGER, INTENT(IN) :: N, LDA, LDV, JPOS
  COMPLEX(KIND=K), INTENT(INOUT) :: A(LDA,N)
  COMPLEX(KIND=K), INTENT(OUT) :: V(LDV,N), WRK(N,N)
  INTEGER, INTENT(INOUT) :: AS, INFO
  REAL(KIND=K) :: AX, TOL
  INTEGER(KIND=INT64) :: TT
  INTEGER :: O, P, Q, R, S, T, U, W, X
  CHARACTER(LEN=11) :: FN
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
  CALL WSCALA(N, A, LDA, AX, AS, R)
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
  O = ICHAR('W')
  CALL WTRACE(N, A, LDA, AX, AS, R, S)
  IF (IAND(INFO, 4) .NE. 0) CALL WTRCOA(N, A, LDA, AS, R, O, U)
  TT = 0_INT64
  ! main loop
  DO R = 1, S
     T = 0
     X = 1
     WRK(X,X) = CMPLX(ZERO, R, K)
     IF (IAND(INFO, 2) .EQ. 0) THEN
        DO P = 1, N-1
           X = X + 1
           WRK(X,X) = CMPLX(ZERO, P, K)
           DO Q = P+1, N
              W = IAND(INFO, 1)
              IF ((P .LE. JPOS) .AND. (Q .GT. JPOS)) W = IOR(W, 2)
              WRK(Q,P) = TOL
              CALL WTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(Q,P), W)
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
              IF (IAND(INFO, 4) .NE. 0) CALL WTRCOA(N, A, LDA, AS, R, O, U)
           END DO
        END DO
     ELSE ! column cyclic
        DO Q = 2, N
           X = X + 1
           WRK(X,X) = CMPLX(ZERO, Q, K)
           DO P = 1, Q-1
              W = IAND(INFO, 1)
              IF ((P .LE. JPOS) .AND. (Q .GT. JPOS)) W = IOR(W, 2)
              WRK(Q,P) = TOL
              CALL WTRANA(N, A, LDA, V, LDV, AX, AS, P, Q, WRK(Q,P), W)
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
              IF (IAND(INFO, 4) .NE. 0) CALL WTRCOA(N, A, LDA, AS, R, O, U)
           END DO
        END DO
     END IF
     CALL WTRACE(N, A, LDA, AX, AS, R, T)
     IF (N .LT. 1000) THEN
        X = -AS
        DO Q = 1, N
           DO P = 1, Q-1
              WRK(P,Q) = CMPLX(SCALE(REAL(A(P,Q)), X), SCALE(AIMAG(A(P,Q)), X), K)
              IF (A(P,Q) .NE. CONJG(A(Q,P))) STOP 'H'
           END DO
           WRK(Q,Q) = CMPLX(SCALE(REAL(A(Q,Q)), X), ZERO, K)
        END DO
        IF (IAND(INFO, 2) .EQ. 0) THEN
           WRITE (FN,'(A,I3.3,A,I2.2,A)') 'W', N, '_', R, '.txt'
        ELSE ! the modified deRijk
           WRITE (FN,'(A,I3.3,A,I2.2,A)') 'W', N, '-', R, '.txt'
        END IF
        OPEN(NEWUNIT=W, IOSTAT=X, FILE=FN, STATUS='REPLACE', ACTION='WRITE', ACCESS='SEQUENTIAL', FORM='FORMATTED')
        IF (N .EQ. 1) THEN
           WRITE (W,'(2(A,ES30.21E4),A)') '(', REAL(WRK(1,1)), ',', AIMAG(WRK(1,1)), ')'
        ELSE ! N > 1
           DO P = 1, N
              DO Q = 1, N-1
                 IF (Q .EQ. 1) THEN
                    WRITE (W,'(2(A,ES30.21E4),A)',ADVANCE='NO') '(', REAL(WRK(P,1)), ',', AIMAG(WRK(P,1)), ')'
                 ELSE ! Q > 1
                    WRITE (W,'(2(A,ES30.21E4),A)',ADVANCE='NO') ' (', REAL(WRK(P,Q)), ',', AIMAG(WRK(P,Q)), ')'
                 END IF
              END DO
              WRITE (W,'(2(A,ES30.21E4),A)') ' (', REAL(WRK(P,N)), ',', AIMAG(WRK(P,N)), ')'
           END DO
        END IF
        CLOSE(UNIT=W, IOSTAT=X)
     END IF
     IF (T .EQ. 0) EXIT
  END DO
  IF (IAND(INFO, 4) .NE. 0) CALL WTRCOA(N, A, LDA, AS, -1, O, U)
  INFO = R
  WRK(1,1) = TT
END SUBROUTINE WJEVDC

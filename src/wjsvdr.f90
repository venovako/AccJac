!  IN: GS = max sweeps, INFO = 0 or 1 (sin => tan) OR 2 (dsc/asc)
! OUT: GS: backscale SV by 2**-GS, INFO: #sweeps
SUBROUTINE WJSVDR(M, N, G, LDG, V, LDV, JPOS, SV, WRK, GS, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE WSCALG(M, N, G, LDG, GX, GS, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: GX
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: G(LDG,N)
       REAL(KIND=REAL128), INTENT(INOUT) :: GX
#endif
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE WSCALG
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WINISV(M, N, G, LDG, V, LDV, JPOS, SV, WRK, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: G(LDG,N)
       COMPLEX(KIND=c_long_double), INTENT(OUT) :: V(LDV,N), WRK(M,N)
       REAL(KIND=c_long_double), INTENT(OUT) :: SV(N)
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: G(LDG,N)
       COMPLEX(KIND=REAL128), INTENT(OUT) :: V(LDV,N), WRK(M,N)
       REAL(KIND=REAL128), INTENT(OUT) :: SV(N)
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WINISV
  END INTERFACE
  INTERFACE
     SUBROUTINE WTRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: G(LDG,N), V(LDV,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: SV(N), GX
       REAL(KIND=c_long_double), INTENT(IN) :: TOL
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: G(LDG,N), V(LDV,N)
       REAL(KIND=REAL128), INTENT(INOUT) :: SV(N), GX
       REAL(KIND=REAL128), INTENT(IN) :: TOL
#endif
       INTEGER, INTENT(INOUT) :: GS, INFO
     END SUBROUTINE WTRANS
  END INTERFACE
  INTERFACE
     SUBROUTINE XTRACK(N, SV, GX, GS, SWP, NTR)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, GS, SWP, NTR
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: SV(N), GX
#else
       REAL(KIND=REAL128), INTENT(IN) :: SV(N), GX
#endif
     END SUBROUTINE XTRACK
  END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, EPS = EPSILON(EPS) / 2
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, JPOS
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N)
  COMPLEX(KIND=K), INTENT(OUT) :: V(LDV,N), WRK(M,N)
  REAL(KIND=K), INTENT(OUT) :: SV(N)
  INTEGER, INTENT(INOUT) :: GS, INFO
  COMPLEX(KIND=K) :: Z
  REAL(KIND=K) :: GX, TOL, X
  INTEGER :: P, Q, R, S, T, W
  IF ((INFO .LT. 0) .OR. (INFO .GT. 7)) INFO = -11
  IF (GS .LT. 0) INFO = -10
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) INFO = -7
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF ((N .LT. 0) .OR. (N .GT. M)) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (N .EQ. 0) RETURN
  S = GS
  ! prescale G
  GX = ZERO
  GS = 0
  R = 0
  CALL WSCALG(M, N, G, LDG, GX, GS, R)
  IF (R .LT. 0) THEN
     INFO = -3
     RETURN
  END IF
  ! init SV, V; sort SV, G
  R = IAND(INFO, 2)
  CALL WINISV(M, N, G, LDG, V, LDV, JPOS, SV, WRK, R)
  IF (R .LT. 0) THEN
     INFO = -9
     RETURN
  END IF
  TOL = M
  TOL = SQRT(TOL) * EPS
  CALL XTRACK(N, SV, GX, GS, -R, S)
  DO R = 1, S
     T = 0
     ! the first diagonal block
     DO P = 1, JPOS-1
        X = ZERO
        W = 0
        DO Q = P, JPOS
           IF (SV(Q) .GT. X) THEN
              X = SV(Q)
              W = Q
           END IF
        END DO
        IF (W .GT. P) THEN
           DO Q = 1, M
              Z = G(Q,P)
              G(Q,P) = G(Q,W)
              G(Q,W) = Z
           END DO
           DO Q = 1, N
              Z = V(Q,P)
              V(Q,P) = V(Q,W)
              V(Q,W) = Z
           END DO
           X = SV(P)
           SV(P) = SV(W)
           SV(W) = X
           T = T + 1
        END IF
        DO Q = P+1, JPOS
           W = IAND(INFO, 1)
           CALL WTRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, W)
           SELECT CASE (W)
           CASE (0)
              CONTINUE
           CASE (1,2,3)
              T = T + 1
           CASE DEFAULT
              INFO = -5
              RETURN
           END SELECT
        END DO
     END DO
     ! the off-diagonal block (hyp)
     DO P = 1, JPOS
        DO Q = JPOS+1, N
           W = IOR(IAND(INFO, 1), 2)
           CALL WTRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, W)
           SELECT CASE (W)
           CASE (0)
              CONTINUE
           CASE (1,2,3)
              T = T + 1
           CASE DEFAULT
              INFO = -5
              RETURN
           END SELECT
        END DO
     END DO
     ! the second diagonal block
     IF (IAND(INFO, 2) .EQ. 0) THEN
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
              DO Q = 1, M
                 Z = G(Q,P)
                 G(Q,P) = G(Q,W)
                 G(Q,W) = Z
              END DO
              DO Q = 1, N
                 Z = V(Q,P)
                 V(Q,P) = V(Q,W)
                 V(Q,W) = Z
              END DO
              X = SV(P)
              SV(P) = SV(W)
              SV(W) = X
              T = T + 1
           END IF
           DO Q = P+1, N
              W = IAND(INFO, 1)
              CALL WTRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, W)
              SELECT CASE (W)
              CASE (0)
                 CONTINUE
              CASE (1,2,3)
                 T = T + 1
              CASE DEFAULT
                 INFO = -5
                 RETURN
              END SELECT
           END DO
        END DO
     ELSE ! asc
        DO Q = N, JPOS+2, -1
           X = ZERO
           W = 0
           DO P = Q, JPOS+1, -1
              IF (SV(P) .GT. X) THEN
                 X = SV(P)
                 W = P
              END IF
           END DO
           IF (W .LT. Q) THEN
              DO P = 1, M
                 Z = G(P,Q)
                 G(P,Q) = G(P,W)
                 G(P,W) = Z
              END DO
              DO P = 1, N
                 Z = V(P,Q)
                 V(P,Q) = V(P,W)
                 V(P,W) = Z
              END DO
              X = SV(Q)
              SV(Q) = SV(W)
              SV(W) = X
              T = T + 1
           END IF
           DO P = Q-1, JPOS+1, -1
              W = IAND(INFO, 1)
              CALL WTRANS(M, N, G, LDG, V, LDV, SV, GX, GS, Q, P, TOL, W)
              SELECT CASE (W)
              CASE (0)
                 CONTINUE
              CASE (1,2,3)
                 T = T + 1
              CASE DEFAULT
                 INFO = -5
                 RETURN
              END SELECT
           END DO
        END DO
     END IF
     CALL XTRACK(N, SV, GX, GS, R, T)
     IF (T .EQ. 0) EXIT
  END DO
  ! rescale U
  IF (R .LE. S) THEN
     DO Q = 1, N
        IF (.NOT. (SV(Q) .GT. ZERO)) THEN
           INFO = -8
           RETURN
        END IF
        IF (SV(Q) .NE. ONE) THEN
           DO P = 1, M
              G(P,Q) = G(P,Q) / SV(Q)
           END DO
        END IF
     END DO
  END IF
  INFO = R
END SUBROUTINE WJSVDR

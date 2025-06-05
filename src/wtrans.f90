! IN: INFO & 1: sin => tan
!     INFO & 2: hyp
!OUT: INFO = 0: notransf
!     INFO = 1: swap only
!     INFO = 2: transf, no downscaling of G and SV
!     INFO = 3: transf with downscaling of G and SV
SUBROUTINE WTRANS(M, N, G, LDG, V, LDV, SV, GX, GS, P, Q, TOL, INFO)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
#ifdef __GFORTRAN__
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif
  INTERFACE
     FUNCTION WSDP(M, X, Y, MX, MY, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
       INTEGER, INTENT(INOUT) :: INFO
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(IN) :: X(M), Y(M)
       REAL(KIND=c_long_double), INTENT(IN) :: MX, MY
       COMPLEX(KIND=c_long_double) :: WSDP
#else
       COMPLEX(KIND=REAL128), INTENT(IN) :: X(M), Y(M)
       REAL(KIND=REAL128), INTENT(IN) :: MX, MY
       COMPLEX(KIND=REAL128) :: WSDP
#endif
     END FUNCTION WSDP
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WGRAM(PNF, QNF, QPS, APP, AQQ, AQPR, AQPI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: PNF, QNF
       COMPLEX(KIND=c_long_double), INTENT(IN) :: QPS
       REAL(KIND=c_long_double), INTENT(OUT) :: APP, AQQ, AQPR, AQPI
#else
       REAL(KIND=REAL128), INTENT(IN) :: PNF, QNF
       COMPLEX(KIND=REAL128), INTENT(IN) :: QPS
       REAL(KIND=REAL128), INTENT(OUT) :: APP, AQQ, AQPR, AQPI
#endif
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE WGRAM
  END INTERFACE
  INTERFACE
     SUBROUTINE WLJU2(A11, A22, A21R, A21I, CS, SNR, SNI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: A11, A22, A21R, A21I
       REAL(KIND=c_long_double), INTENT(OUT) :: CS, SNR, SNI
#else
       REAL(KIND=REAL128), INTENT(IN) :: A11, A22, A21R, A21I
       REAL(KIND=REAL128), INTENT(OUT) :: CS, SNR, SNI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WLJU2
  END INTERFACE
  INTERFACE
     SUBROUTINE WLJV2(A11, A22, A21R, A21I, CH, SHR, SHI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: A11, A22, A21R, A21I
       REAL(KIND=c_long_double), INTENT(OUT) :: CH, SHR, SHI
#else
       REAL(KIND=REAL128), INTENT(IN) :: A11, A22, A21R, A21I
       REAL(KIND=REAL128), INTENT(OUT) :: CH, SHR, SHI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WLJV2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTVT(N, X, Y, CS, SNR, SNI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=c_long_double), INTENT(IN) :: CS, SNR, SNI
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=REAL128), INTENT(IN) :: CS, SNR, SNI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTVT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WRTVH(N, X, Y, CH, SHR, SHI, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=c_long_double), INTENT(IN) :: CH, SHR, SHI
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: X(N), Y(N)
       REAL(KIND=REAL128), INTENT(IN) :: CH, SHR, SHI
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WRTVH
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WROTT(M, X, Y, CS, SNR, SNI, GX, MX, MY, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: X(M), Y(M)
       REAL(KIND=c_long_double), INTENT(IN) :: CS, SNR, SNI
       REAL(KIND=c_long_double), INTENT(INOUT) :: GX, MX, MY
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: X(M), Y(M)
       REAL(KIND=REAL128), INTENT(IN) :: CS, SNR, SNI
       REAL(KIND=REAL128), INTENT(INOUT) :: GX, MX, MY
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WROTT
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WROTH(M, X, Y, CH, SHR, SHI, GX, MX, MY, INFO)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(INOUT) :: X(M), Y(M)
       REAL(KIND=c_long_double), INTENT(IN) :: CH, SHR, SHI
       REAL(KIND=c_long_double), INTENT(INOUT) :: GX, MX, MY
#else
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: X(M), Y(M)
       REAL(KIND=REAL128), INTENT(IN) :: CH, SHR, SHI
       REAL(KIND=REAL128), INTENT(INOUT) :: GX, MX, MY
#endif
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE WROTH
  END INTERFACE
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
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, P, Q
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N), V(LDV,N)
  REAL(KIND=K), INTENT(INOUT) :: SV(N), GX
  REAL(KIND=K), INTENT(IN) :: TOL
  INTEGER, INTENT(INOUT) :: GS, INFO
  COMPLEX(KIND=K) :: QPS, Z
  REAL(KIND=K) :: APP, AQQ, AQPR, AQPI, C, SR, SI, T
  INTEGER :: I, J, L
  IF ((INFO .LT. 0) .OR. (INFO .GT. 3)) INFO = -13
  IF (TOL .LT. ZERO) INFO = -12
  IF ((Q .LE. 0) .OR. (Q .GT. N)) INFO = -11
  IF ((P .LE. 0) .OR. (P .GT. N)) INFO = -10
  IF (GX .LT. ZERO) INFO = -8
  IF (LDV .LT. N) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF ((N .LT. 0) .OR. (N .GT. M)) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (M .EQ. 0) RETURN
  I = 0
  QPS = WSDP(M, G(1,Q), G(1,P), SV(Q), SV(P), I)
  IF (I .NE. 0) THEN
     INFO = -3
     RETURN
  END IF
  T = CR_HYPOT(REAL(QPS), AIMAG(QPS))
  IF (.NOT. (T .LE. HUGE(T))) THEN
     INFO = -3
     RETURN
  END IF
  L = INFO
  IF (T .LT. TOL) THEN
     IF ((IAND(L, 2) .EQ. 0) .AND. (SV(P) .LT. SV(Q))) THEN
        DO I = 1, M
           Z = G(I,P)
           G(I,P) = G(I,Q)
           G(I,Q) = Z
        END DO
        DO I = 1, N
           Z = V(I,P)
           V(I,P) = V(I,Q)
           V(I,Q) = Z
        END DO
        T = SV(P)
        SV(P) = SV(Q)
        SV(Q) = T
        INFO = 1
     ELSE ! no swap
        INFO = 0
     END IF
     RETURN
  ELSE ! transform
     INFO = 2
  END IF
  J = 0
  CALL WGRAM(SV(P), SV(Q), QPS, APP, AQQ, AQPR, AQPI, J)
  IF (J .LE. -HUGE(J)) THEN
     INFO = -7
     RETURN
  END IF
  J = IAND(L, 2)
  I = IAND(L, 1)
  T = GX
  IF (J .EQ. 0) THEN
     CALL WLJU2(APP, AQQ, AQPR, AQPI, C, SR, SI, I)
     CALL WROTT(M, G(1,P), G(1,Q), C, SR, SI, T, SV(P), SV(Q), I)
     CALL WRTVT(N, V(1,P), V(1,Q), C, SR, SI, I)
  ELSE ! hyp
     CALL WLJV2(APP, AQQ, AQPR, AQPI, C, SR, SI, I)
     CALL WROTH(M, G(1,P), G(1,Q), C, SR, SI, T, SV(P), SV(Q), I)
     CALL WRTVH(N, V(1,P), V(1,Q), C, SR, SI, I)
  END IF
  IF (I .LT. 0) THEN
     INFO = -8
  ELSE IF (IAND(I, 4) .NE. 0) THEN
     IF (IAND(I, 8) .EQ. 0) THEN
        INFO = 0
     ELSE ! swap
        INFO = 1
     END IF
  ELSE IF (T .GT. GX) THEN
     GX = T
     I = 1
     CALL WSCALG(M, N, G, LDG, GX, GS, I)
     IF (I .LT. 0) THEN
        INFO = -9
        RETURN
     END IF
     IF (I .GT. 0) THEN
        I = -I
        DO J = 1, N
           SV(J) = SCALE(SV(J), I)
        END DO
        INFO = 3
     END IF
  END IF
END SUBROUTINE WTRANS

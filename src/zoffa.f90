FUNCTION ZOFFA(M, N, G, LDG, GS, WRK)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION C_FMA(X, Y, Z) BIND(C,NAME='fmal')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y, Z
       REAL(KIND=c_long_double) :: C_FMA
     END FUNCTION C_FMA
  END INTERFACE
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  INTEGER, PARAMETER :: KK = c_long_double
#else
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
  IMPLICIT NONE
#define C_FMA IEEE_FMA
#define CR_HYPOT HYPOT
  INTEGER, PARAMETER :: KK = REAL128
#endif
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=KK), PARAMETER :: ZERO = 0.0_KK, TWO = 2.0_KK, SQRT2 = SQRT(TWO)
  INTEGER, INTENT(IN) :: M, N, LDG, GS
  COMPLEX(KIND=K), INTENT(IN) :: G(LDG,N)
  ! this is an ugly HACK, but it is simple to allocate twice the space for WRK
  ! and pretend WRK is of the KK kind, provided that the alignment is correct
  COMPLEX(KIND=KK), INTENT(OUT) :: WRK(M,N)
  REAL(KIND=K) :: ZOFFA
  COMPLEX(KIND=KK) :: D, X, Y
  REAL(KIND=KK) :: O
  INTEGER :: I, J, L
  L = -GS
  DO J = 1, N
     DO I = 1, M
        WRK(I,J) = CMPLX(SCALE(REAL(REAL(G(I,J)), KK), L), SCALE(REAL(AIMAG(G(I,J)), KK), L), KK)
     END DO
  END DO
  O = ZERO
  DO J = 2, N
     DO I = 1, J-1
        D = ZERO
        DO L = 1, M
           X = CONJG(WRK(L,I))
           Y = WRK(L,J)
           D = CMPLX(&
                C_FMA(REAL(X), REAL(Y), C_FMA(-AIMAG(X), AIMAG(Y), REAL(D))),&
                C_FMA(REAL(X), AIMAG(Y), C_FMA(AIMAG(X), REAL(Y), AIMAG(D))), KK)
        END DO
        O = CR_HYPOT(O, CR_HYPOT(REAL(D), AIMAG(D)))
     END DO
  END DO
  O = O * SQRT2
  ZOFFA = REAL(O, K)
END FUNCTION ZOFFA

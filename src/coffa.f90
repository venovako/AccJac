FUNCTION COFFA(M, N, G, LDG, GS, WRK)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION ZFMA(A, B, C)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       COMPLEX(KIND=REAL64), INTENT(IN) :: A, B, C
       COMPLEX(KIND=REAL64) :: ZFMA
     END FUNCTION ZFMA
  END INTERFACE
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       IMPLICIT NONE
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL32, KK = REAL64
  REAL(KIND=KK), PARAMETER :: ZERO = 0.0_KK, TWO = 2.0_KK, SQRT2 = SQRT(TWO)
  INTEGER, INTENT(IN) :: M, N, LDG, GS
  COMPLEX(KIND=K), INTENT(IN) :: G(LDG,N)
  ! this is an ugly HACK, but it is simple to allocate twice the space for WRK
  ! and pretend WRK is of the KK kind, provided that the alignment is correct
  COMPLEX(KIND=KK), INTENT(OUT) :: WRK(M,N)
  REAL(KIND=K) :: COFFA
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
           D = ZFMA(X, Y, D)
        END DO
        O = CR_HYPOT(O, CR_HYPOT(REAL(D), AIMAG(D)))
     END DO
  END DO
  O = O * SQRT2
  COFFA = REAL(O, K)
END FUNCTION COFFA

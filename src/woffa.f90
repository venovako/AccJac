FUNCTION WOFFA(M, N, G, LDG, GS, WRK)
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = 10, KK = REAL128
  REAL(KIND=KK), PARAMETER :: ZERO = 0.0_KK, TWO = 2.0_KK, SQRT2 = SQRT(TWO)
  INTEGER, INTENT(IN) :: M, N, LDG, GS
  COMPLEX(KIND=K), INTENT(IN) :: G(LDG,N)
  ! this is an ugly HACK, but it is simple to allocate twice the space for WRK
  ! and pretend WRK is of the KK kind, provided that the alignment is correct
  COMPLEX(KIND=KK), INTENT(OUT) :: WRK(M,N)
  REAL(KIND=K) :: WOFFA
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
                IEEE_FMA(REAL(X), REAL(Y), IEEE_FMA(-AIMAG(X), AIMAG(Y), REAL(D))),&
                IEEE_FMA(REAL(X), AIMAG(Y), IEEE_FMA(AIMAG(X), REAL(Y), AIMAG(D))), KK)
        END DO
        O = HYPOT(O, HYPOT(REAL(D), AIMAG(D)))
     END DO
  END DO
  O = O * SQRT2
  WOFFA = REAL(O, K)
END FUNCTION WOFFA

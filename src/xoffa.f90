FUNCTION XOFFA(M, N, G, LDG, GS, WRK)
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = c_long_double, KK = REAL128
  REAL(KIND=KK), PARAMETER :: ZERO = 0.0_KK, TWO = 2.0_KK, SQRT2 = SQRT(TWO)
  INTEGER, INTENT(IN) :: M, N, LDG, GS
  REAL(KIND=K), INTENT(IN) :: G(LDG,N)
  ! this is an ugly HACK, but it is simple to allocate twice the space for WRK
  ! and pretend WRK is of the KK kind, provided that the alignment is correct
  REAL(KIND=KK), INTENT(OUT) :: WRK(M,N)
  REAL(KIND=K) :: XOFFA
  REAL(KIND=KK) :: D, O
  INTEGER :: I, J, L
  L = -GS
  DO J = 1, N
     DO I = 1, M
        WRK(I,J) = SCALE(REAL(G(I,J), KK), L)
     END DO
  END DO
  O = ZERO
  DO J = 2, N
     DO I = 1, J-1
        D = ZERO
        DO L = 1, M
           D = IEEE_FMA(WRK(L,I), WRK(L,J), D)
        END DO
        O = HYPOT(O, D)
     END DO
  END DO
  O = O * SQRT2
  XOFFA = REAL(O, K)
END FUNCTION XOFFA

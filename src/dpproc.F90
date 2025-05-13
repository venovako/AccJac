PROGRAM DPPROC
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, REAL64
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, REAL64, REAL128
#endif
  IMPLICIT NONE
#ifdef __GFORTRAN__
  INTERFACE
     PURE FUNCTION HYPOTX(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: HYPOTX
     END FUNCTION HYPOTX
  END INTERFACE
  INTEGER, PARAMETER :: KK = c_long_double
#else
#define HYPOTX HYPOT
  INTEGER, PARAMETER :: KK = REAL128
#endif
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=KK), PARAMETER :: XZERO = 0.0_KK, SQRT2 = SQRT(2.0_KK)
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  CHARACTER(LEN=11) :: FN
  CHARACTER :: T
  REAL(KIND=KK) :: X, Y
  INTEGER :: I, J, L, N, S, U, V
  REAL(KIND=K), ALLOCATABLE :: W1(:,:), A1(:,:), W3(:,:), A3(:,:)
  ! read the command line arguments
  L = COMMAND_ARGUMENT_COUNT()
  IF (L .NE. 2) STOP 'dpproc.exe N S'
  CALL GET_COMMAND_ARGUMENT(1, FN)
  READ (FN,*) N
  IF (N .LT. 0) THEN
     N = -N
     T = 'D'
  ELSE IF (N .GT. 0) THEN
     T = 'd'
  ELSE ! N = 0
     STOP 'N'
  END IF
  CALL GET_COMMAND_ARGUMENT(2, FN)
  READ (FN,*) S
  IF (S .LE. 0) STOP 'S'
  ALLOCATE(W1(N,N))
  ALLOCATE(A1(N,N))
  ALLOCATE(W3(N,N))
  ALLOCATE(A3(N,N))
  WRITE (OUTPUT_UNIT,'(A)') '"SWEEP", "P", "Q", "maxRE(TAN[H])@PQ", "maxRE(A)"'
  DO L = 1, S
     WRITE (FN,'(A,I3.3,A,I2.2,A)') T, N, '_', L, '.txt'
     OPEN(NEWUNIT=U, IOSTAT=V, FILE=FN, STATUS='OLD', ACTION='READ', ACCESS='SEQUENTIAL', FORM='FORMATTED')
     IF (V .NE. 0) STOP 'OPEN(1)'
     DO I = 1, N
        READ (U,*) (W1(I,J), J=1,N)
     END DO
     CLOSE(UNIT=U, IOSTAT=V)
     IF (V .NE. 0) STOP 'CLOSE(1)'
     DO J = 1, N
        DO I = 1, J
           A1(I,J) = W1(I,J)
           W1(I,J) = ZERO
        END DO
     END DO
     DO J = 1, N-1
        DO I = J+1, N
           A1(I,J) = A1(J,I)
        END DO
     END DO
     WRITE (FN,'(A,I3.3,A,I2.2,A)') T, N, '-', L, '.txt'
     OPEN(NEWUNIT=U, IOSTAT=V, FILE=FN, STATUS='OLD', ACTION='READ', ACCESS='SEQUENTIAL', FORM='FORMATTED')
     IF (V .NE. 0) STOP 'OPEN(3)'
     DO I = 1, N
        READ (U,*) (W3(I,J), J=1,N)
     END DO
     CLOSE(UNIT=U, IOSTAT=V)
     IF (V .NE. 0) STOP 'CLOSE(3)'
     DO J = 1, N
        DO I = 1, J
           A3(I,J) = W3(I,J)
           W3(I,J) = ZERO
        END DO
     END DO
     DO J = 1, N-1
        DO I = J+1, N
           A3(I,J) = A3(J,I)
        END DO
     END DO
     X = XZERO
     U = 0
     V = 0
     DO J = 1, N-1
        DO I = J+1, N
           Y = ABS(REAL(W1(I,J), KK) - REAL(W3(I,J), KK))
           IF (Y .NE. XZERO) Y = Y / ABS(W1(I,J))
           IF (Y .GT. X) THEN
              X = Y
              U = J
              V = I
           END IF
        END DO
     END DO
     WRITE (OUTPUT_UNIT,'(I2,A,2(I4,A),ES25.17E3,A)',ADVANCE='NO') L, ',', U, ',', V, ',', X, ','
     FLUSH(OUTPUT_UNIT)
     X = XZERO
     DO J = 1, N
        DO I = 1, N
           Y = REAL(A1(I,J), KK) - REAL(A3(I,J), KK)
           X = HYPOTX(X, Y)
        END DO
     END DO
     IF (X .NE. XZERO) THEN
        Y = XZERO
        DO J = 2, N
           DO I = 1, J-1
              Y = HYPOTX(Y, REAL(A1(I,J), KK))
           END DO
           Y = Y * SQRT2
        END DO
        DO J = 1, N
           Y = HYPOTX(Y, REAL(A1(J,J), KK))
        END DO
        X = X / Y
     END IF
     WRITE (OUTPUT_UNIT,'(ES25.17E3)') X
  END DO
  DEALLOCATE(A3)
  DEALLOCATE(W3)
  DEALLOCATE(A1)
  DEALLOCATE(W1)
END PROGRAM DPPROC

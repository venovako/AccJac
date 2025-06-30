PROGRAM SPPROC
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, REAL32, REAL64
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       IMPLICIT NONE
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  INTEGER, PARAMETER :: KK = REAL64
  INTEGER, PARAMETER :: K = REAL32
  REAL(KIND=KK), PARAMETER :: XZERO = 0.0_KK, SQRT2 = SQRT(2.0_KK)
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  CHARACTER(LEN=11) :: FN
  CHARACTER :: T
  REAL(KIND=KK) :: X, Y
  INTEGER :: I, J, L, N, S, U, V
  REAL(KIND=K), ALLOCATABLE :: W1(:,:), A1(:,:), W3(:,:), A3(:,:)
  ! read the command line arguments
  L = COMMAND_ARGUMENT_COUNT()
  IF (L .NE. 2) STOP 'spproc.exe N S'
  CALL GET_COMMAND_ARGUMENT(1, FN)
  READ (FN,*) N
  IF (N .LT. 0) THEN
     N = -N
     T = 'S'
  ELSE IF (N .GT. 0) THEN
     T = 's'
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
  WRITE (OUTPUT_UNIT,'(A)') '"SWEEP", "maxRE(TAN[H])", "maxRE(A)"'
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
     DO J = 1, N-1
        DO I = J+1, N
           Y = ABS(REAL(W1(I,J), KK) - REAL(W3(I,J), KK))
           IF (Y .NE. XZERO) Y = Y / ABS(W1(I,J))
           IF (Y .GT. X) X = Y
        END DO
     END DO
     WRITE (OUTPUT_UNIT,'(I2,A,ES16.9E2,A)',ADVANCE='NO') L, ',', X, ','
     FLUSH(OUTPUT_UNIT)
     X = XZERO
     DO J = 1, N
        DO I = 1, N
           Y = REAL(A1(I,J), KK) - REAL(A3(I,J), KK)
           X = CR_HYPOT(X, Y)
        END DO
     END DO
     IF (X .NE. XZERO) THEN
        Y = XZERO
        DO J = 2, N
           DO I = 1, J-1
              Y = CR_HYPOT(Y, REAL(A1(I,J), KK))
           END DO
        END DO
        Y = Y * SQRT2
        DO J = 1, N
           Y = CR_HYPOT(Y, REAL(A1(J,J), KK))
        END DO
        X = X / Y
     END IF
     WRITE (OUTPUT_UNIT,'(ES16.9E2)') X
  END DO
  DEALLOCATE(A3)
  DEALLOCATE(W3)
  DEALLOCATE(A1)
  DEALLOCATE(W1)
END PROGRAM SPPROC

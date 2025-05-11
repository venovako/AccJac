PROGRAM DPPROC
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, REAL64
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       IMPLICIT NONE
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  CHARACTER(LEN=11) :: FN
  REAL(KIND=K) :: XW
  INTEGER :: I, J, L, N, S, U, V
  REAL(KIND=K), ALLOCATABLE :: W1(:,:), A1(:,:), W3(:,:), A3(:,:)
  REAL(KIND=K), EXTERNAL :: DNRMF
  ! read the command line arguments
  L = COMMAND_ARGUMENT_COUNT()
  IF (L .NE. 2) STOP 'dpproc.exe N S'
  CALL GET_COMMAND_ARGUMENT(1, FN)
  READ (FN,*) N
  IF (N .LE. 0) STOP 'N'
  CALL GET_COMMAND_ARGUMENT(2, FN)
  READ (FN,*) S
  IF (S .LE. 0) STOP 'S'
  ALLOCATE(W1(N,N))
  ALLOCATE(A1(N,N))
  ALLOCATE(W3(N,N))
  ALLOCATE(A3(N,N))
  DO L = 1, S
     WRITE (FN,'(A,I3.3,A,I2.2,A)') 'd', N, '_', L, '.txt'
     OPEN(NEWUNIT=U, IOSTAT=J, FILE=FN, STATUS='OLD', ACTION='READ', ACCESS='SEQUENTIAL', FORM='FORMATTED')
     IF (J .NE. 0) STOP 'OPEN(1)'
     DO I = 1, N
        READ (U,*) (W1(I,J), J=1,N)
     END DO
     CLOSE(UNIT=U, IOSTAT=J)
     IF (J .NE. 0) STOP 'CLOSE(1)'
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
     WRITE (FN,'(A,I3.3,A,I2.2,A)') 'd', N, '-', L, '.txt'
     OPEN(NEWUNIT=U, IOSTAT=J, FILE=FN, STATUS='OLD', ACTION='READ', ACCESS='SEQUENTIAL', FORM='FORMATTED')
     IF (J .NE. 0) STOP 'OPEN(3)'
     DO I = 1, N
        READ (U,*) (W3(I,J), J=1,N)
     END DO
     CLOSE(UNIT=U, IOSTAT=J)
     IF (J .NE. 0) STOP 'CLOSE(3)'
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
     XW = ZERO
     U = 0
     V = 0
     DO J = 1, N-1
        DO I = J+1, N
           W1(J,I) = ABS(W1(I,J) - W3(I,J))
           IF (W1(J,I) .NE. ZERO) W3(J,I) = W1(J,I) / ABS(W1(I,J))
           IF (W3(J,I) .GT. XW) THEN
              XW = MAX(XW, W3(J,I))
              U = J
              V = I
           END IF
        END DO
     END DO
     WRITE (OUTPUT_UNIT,'(I2,I4,I4,ES25.17E3)',ADVANCE='NO') L, U, V, XW
     FLUSH(OUTPUT_UNIT)
     XW = ZERO
     DO J = 1, N
        DO I = 1, N
           A3(I,J) = A1(I,J) - A3(I,J)
           XW = CR_HYPOT(XW, A3(I,J))
        END DO
     END DO
     V = N * N
     IF (XW .NE. ZERO) XW = XW / DNRMF(V, A1)
     WRITE (OUTPUT_UNIT,'(ES25.17E3)') XW
  END DO
  DEALLOCATE(A3)
  DEALLOCATE(W3)
  DEALLOCATE(A1)
  DEALLOCATE(W1)
END PROGRAM DPPROC

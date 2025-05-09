PROGRAM PAST
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
  IMPLICIT NONE
  CHARACTER(LEN=20) :: CLA
  INTEGER :: N, INFO, I, J
  INTEGER, ALLOCATABLE :: L(:,:), U(:,:), S(:,:)
  EXTERNAL :: PASCAL
  INFO = COMMAND_ARGUMENT_COUNT()
  IF (INFO .NE. 1) STOP 'past.exe N'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) N
  IF (N .LE. 0) STOP 'N <= 0'
  ALLOCATE(L(N,N))
  ALLOCATE(U(N,N))
  ALLOCATE(S(N,N))
  CALL PASCAL(N, L, U, S, INFO)
  IF (INFO .NE. 0) STOP 'PASCAL'
  INFO = S(N,N)
  IF (INFO .LT. 10) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I2)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I2)') S(I,N)
     END DO
  ELSE IF (INFO .LT. 100) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I3)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I3)') S(I,N)
     END DO
  ELSE IF (INFO .LT. 1000) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I4)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I4)') S(I,N)
     END DO
  ELSE IF (INFO .LT. 10000) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I5)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I5)') S(I,N)
     END DO
  ELSE IF (INFO .LT. 100000) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I6)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I6)') S(I,N)
     END DO
  ELSE IF (INFO .LT. 1000000) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I7)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I7)') S(I,N)
     END DO
  ELSE IF (INFO .LT. 10000000) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I8)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I8)') S(I,N)
     END DO
  ELSE IF (INFO .LT. 100000000) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I9)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I9)') S(I,N)
     END DO
  ELSE IF (INFO .LT. 1000000000) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I10)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I10)') S(I,N)
     END DO
  ELSE ! INFO > 1000000000
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I11)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I11)') S(I,N)
     END DO
  END IF
  DEALLOCATE(S)
  DEALLOCATE(U)
  DEALLOCATE(L)
END PROGRAM PAST

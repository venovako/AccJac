PROGRAM PAST
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, OUTPUT_UNIT
  IMPLICIT NONE
  CHARACTER(LEN=20) :: CLA
  INTEGER :: N, INFO, I, J
  INTEGER(KIND=INT64), ALLOCATABLE :: L(:,:), U(:,:), S(:,:)
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
  IF (S(N,N) .LT. 10_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I2)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I2)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 100_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I3)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I3)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 1000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I4)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I4)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 10000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I5)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I5)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 100000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I6)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I6)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 1000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I7)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I7)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 10000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I8)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I8)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 100000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I9)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I9)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 1000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I10)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I10)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 10000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I11)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I11)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 100000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I12)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I12)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 1000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I13)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I13)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 10000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I14)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I14)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 100000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I15)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I15)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 1000000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I16)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I16)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 10000000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I17)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I17)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 100000000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I18)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I18)') S(I,N)
     END DO
  ELSE IF (S(N,N) .LT. 1000000000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I19)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I19)') S(I,N)
     END DO
  ELSE ! INFO > 1000000000000000000
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I20)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I20)') S(I,N)
     END DO
  END IF
  DEALLOCATE(S)
  DEALLOCATE(U)
  DEALLOCATE(L)
END PROGRAM PAST

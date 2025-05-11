PROGRAM PAST
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, OUTPUT_UNIT, REAL64
  IMPLICIT NONE
  CHARACTER(LEN=256) :: CLA
  INTEGER(KIND=INT64) :: K, M
  INTEGER :: N, I, J
  INTEGER(KIND=INT64), ALLOCATABLE :: L(:,:), U(:,:), S(:,:)
  REAL(KIND=REAL64), ALLOCATABLE :: A(:,:)
  REAL(KIND=c_long_double), ALLOCATABLE :: AA(:,:)
  EXTERNAL :: PASCAL
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .NE. 2) STOP 'past.exe N FN'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) N
  IF (N .LE. 0) STOP 'N <= 0'
  CALL GET_COMMAND_ARGUMENT(2, CLA)
  IF (LEN_TRIM(CLA) .LE. 0) STOP 'FN empty'
  ALLOCATE(L(N,N))
  ALLOCATE(U(N,N))
  ALLOCATE(S(N,N))
  CALL PASCAL(N, L, U, S, I)
  IF (I .NE. 0) STOP 'PASCAL'
  K = S(N,N)
  IF (K .LT. 10_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I2)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I2)') S(I,N)
     END DO
  ELSE IF (K .LT. 100_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I3)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I3)') S(I,N)
     END DO
  ELSE IF (K .LT. 1000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I4)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I4)') S(I,N)
     END DO
  ELSE IF (K .LT. 10000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I5)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I5)') S(I,N)
     END DO
  ELSE IF (K .LT. 100000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I6)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I6)') S(I,N)
     END DO
  ELSE IF (K .LT. 1000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I7)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I7)') S(I,N)
     END DO
  ELSE IF (K .LT. 10000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I8)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I8)') S(I,N)
     END DO
  ELSE IF (K .LT. 100000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I9)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I9)') S(I,N)
     END DO
  ELSE IF (K .LT. 1000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I10)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I10)') S(I,N)
     END DO
  ELSE IF (K .LT. 10000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I11)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I11)') S(I,N)
     END DO
  ELSE IF (K .LT. 100000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I12)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I12)') S(I,N)
     END DO
  ELSE IF (K .LT. 1000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I13)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I13)') S(I,N)
     END DO
  ELSE IF (K .LT. 10000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I14)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I14)') S(I,N)
     END DO
  ELSE IF (K .LT. 100000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I15)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I15)') S(I,N)
     END DO
  ELSE IF (K .LT. 1000000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I16)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I16)') S(I,N)
     END DO
  ELSE IF (K .LT. 10000000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I17)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I17)') S(I,N)
     END DO
  ELSE IF (K .LT. 100000000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I18)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I18)') S(I,N)
     END DO
  ELSE IF (K .LT. 1000000000000000000_INT64) THEN
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I19)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I19)') S(I,N)
     END DO
  ELSE ! K >= 1000000000000000000
     DO I = 1, N
        DO J = 1, N-1
           WRITE (OUTPUT_UNIT,'(I20)',ADVANCE='NO') S(I,J)
        END DO
        WRITE (OUTPUT_UNIT,'(I20)') S(I,N)
     END DO
  END IF
  M = ISHFT(1_INT64, 53)
  IF (K .LT. M) THEN
     ALLOCATE(A(N,N))
     DO J = 1, N
        DO I = 1, N
           A(I,J) = L(I,J)
        END DO
     END DO
  ELSE ! K >= M
     ALLOCATE(AA(N,N))
     DO J = 1, N
        DO I = 1, N
           AA(I,J) = L(I,J)
        END DO
     END DO
  END IF
  CALL BFOPEN(TRIM(CLA)//'.Y', 'WO', I, J)
  IF (J .NE. 0) STOP 'BFOPEN(Y)'
  IF (K .LT. M) THEN
     WRITE (UNIT=I,IOSTAT=J) A
  ELSE ! K >= M
     WRITE (UNIT=I,IOSTAT=J) AA
  END IF
  IF (J .NE. 0) STOP 'WRITE(Y)'
  CLOSE(I)
  IF (K .LT. M) THEN
     DO J = 1, N
        DO I = 1, N
           A(I,J) = S(I,J)
        END DO
     END DO
  ELSE ! K >= M
     DO J = 1, N
        DO I = 1, N
           AA(I,J) = S(I,J)
        END DO
     END DO
  END IF
  CALL BFOPEN(TRIM(CLA)//'.A', 'WO', I, J)
  IF (J .NE. 0) STOP 'BFOPEN(A)'
  IF (K .LT. M) THEN
     WRITE (UNIT=I,IOSTAT=J) A
     DEALLOCATE(A)
  ELSE ! K >= M
     WRITE (UNIT=I,IOSTAT=J) AA
     DEALLOCATE(AA)
  END IF
  IF (J .NE. 0) STOP 'WRITE(A)'
  CLOSE(I)
  DEALLOCATE(S)
  DEALLOCATE(U)
  DEALLOCATE(L)
END PROGRAM PAST

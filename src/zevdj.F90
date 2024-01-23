PROGRAM ZEVDJ
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, REAL64, REAL128
  IMPLICIT NONE

  INTEGER, PARAMETER :: CLAL = 256
  CHARACTER(LEN=CLAL) :: CLA
  COMPLEX(REAL64), ALLOCATABLE :: A(:,:), U(:,:)
  REAL(REAL64) :: RL
  REAL(REAL128) :: XL
  INTEGER :: JOB, N, LDA, LDU, S, INFO, I, J

  EXTERNAL :: ZJAEVD

  J = CLAL
  CALL GET_COMMAND_ARGUMENT(3, CLA, J, INFO)
  IF (INFO .NE. 0) STOP 'invalid command line argument 3 (job spec)'
  READ (CLA,*) JOB

  J = CLAL
  CALL GET_COMMAND_ARGUMENT(2, CLA, J, INFO)
  IF (INFO .NE. 0) STOP 'invalid command line argument 2 (matrix order)'
  READ (CLA,*) N
  IF (N .GE. 0) THEN
     S = 0
  ELSE ! debugging output
     N = -N
     IF (ERROR_UNIT .LT. 0) THEN
        S = -ERROR_UNIT
     ELSE ! ERROR_UNIT .GE. 0
        S = -ERROR_UNIT - 1
     END IF
  END IF

  J = CLAL
  CALL GET_COMMAND_ARGUMENT(1, CLA, J, INFO)
  IF (INFO .NE. 0) STOP 'invalid command line argument 1 (file basename)'

  LDA = N
  ALLOCATE(A(LDA,N))
  OPEN(NEWUNIT=J, IOSTAT=INFO, FILE=TRIM(CLA)//'.A', STATUS='OLD', ACTION='READ', ACCESS='STREAM', FORM='UNFORMATTED')
  IF (INFO .NE. 0) STOP 'error opening A'
  READ (UNIT=J, IOSTAT=INFO) A
  IF (INFO .NE. 0) STOP 'error reading A'
  CLOSE(J)
  IF ((S .NE. 0) .AND. (N .LT. 10)) THEN
     DO I = 1, N
        DO J = 1, N
           WRITE (ERROR_UNIT,'(2(A,ES25.17E3))',ADVANCE='NO') '(', REAL(A(I,J)), ',', AIMAG(A(I,J))
           IF (J .EQ. N) THEN
              WRITE (ERROR_UNIT,'(A)') ')'
           ELSE ! J .LT. N
              WRITE (ERROR_UNIT,'(A)',ADVANCE='NO') ') '
           END IF
        END DO
     END DO
  END IF

  LDU = N
  IF (IAND(JOB, 1) .NE. 0) ALLOCATE(U(LDU,N))

  INFO = 0
  CALL ZJAEVD(JOB, N, A, LDA, U, LDU, S, INFO)
  WRITE (ERROR_UNIT,'(A,I5)') '[INFO] S=', S
  WRITE (ERROR_UNIT,'(A,I6)') 'INFO=', INFO
  IF (INFO .LT. 0) STOP
  IF (ALLOCATED(U)) THEN
     OPEN(NEWUNIT=J, IOSTAT=INFO, FILE=TRIM(CLA)//'.U', STATUS='REPLACE', ACTION='WRITE', ACCESS='STREAM', FORM='UNFORMATTED')
     IF (INFO .NE. 0) STOP 'error opening U'
     WRITE (UNIT=J, IOSTAT=INFO) U
     IF (INFO .NE. 0) STOP 'error writing U'
     CLOSE(J)
     DEALLOCATE(U)
  END IF
  OPEN(NEWUNIT=J, IOSTAT=INFO, FILE=TRIM(CLA)//'.E', STATUS='REPLACE', ACTION='WRITE', ACCESS='STREAM', FORM='UNFORMATTED')
  IF (INFO .NE. 0) STOP 'error opening E'
  IF (S .EQ. 0) THEN
     DO I = 1, N
        RL = REAL(A(I,I))
        WRITE (OUTPUT_UNIT,8) RL
        WRITE (UNIT=J, IOSTAT=INFO) RL
        IF (INFO .NE. 0) STOP 'error writing E'
     END DO
  ELSE ! scale diag(A)
     DO I = 1, N
        RL = REAL(A(I,I))
        XL = REAL(RL, REAL128)
        XL = SCALE(XL, S)
        RL = SCALE(RL, S)
        WRITE (OUTPUT_UNIT,9) XL
        WRITE (UNIT=J, IOSTAT=INFO) RL
        IF (INFO .NE. 0) STOP 'error writing E'
     END DO
  END IF
  IF (ALLOCATED(A)) DEALLOCATE(A)
8 FORMAT(ES25.17E3)
9 FORMAT(ES45.36E4)
END PROGRAM ZEVDJ

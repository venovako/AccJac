PROGRAM XJEVDT
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#endif
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, OUTPUT_UNIT, REAL128
  IMPLICIT NONE
  INTERFACE
#ifdef _OPENMP
     SUBROUTINE QMMMSQ(M, N, A, LDA, B, LDB, C, LDC)
#else
     PURE SUBROUTINE QMMMSQ(M, N, A, LDA, B, LDB, C, LDC)
#endif
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDA, LDB, LDC
       REAL(KIND=REAL128), INTENT(IN) :: A(LDA,M), B(LDB,N)
       REAL(KIND=REAL128), INTENT(INOUT) :: C(LDC,N)
     END SUBROUTINE QMMMSQ
  END INTERFACE
#define HYPOTX HYPOT
  INTEGER, PARAMETER :: KK = REAL128
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=KK), PARAMETER :: XZERO = 0.0_KK
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=KK) :: W, M
  INTEGER :: N, JPOS, INFO, I, J, L, O, AS
  REAL(KIND=K), ALLOCATABLE :: A(:,:), V(:,:), WRK(:,:)
  REAL(KIND=KK), ALLOCATABLE :: X(:,:), Y(:,:), Z(:,:)
  EXTERNAL :: BFOPEN, XJEVDC, XJEVDR
  ! read the command line arguments
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .NE. 4) STOP 'xjevdt.exe N JPOS OPTS FILE'
  I = 0
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) N
  IF (N .LT. 0) THEN
     N = -N
     I = 4
  ELSE IF (N .EQ. 0) THEN
     STOP 'N'
  END IF
  CALL GET_COMMAND_ARGUMENT(2, CLA)
  READ (CLA,*) JPOS
  CALL GET_COMMAND_ARGUMENT(3, CLA)
  READ (CLA,*) O
  INFO = IOR(IAND(O, 3), I)
  SELECT CASE (O)
  CASE (0,1,2,3)
     L = 0
  CASE (4,5,6,7)
     L = 1
  CASE DEFAULT
     STOP 'OPTS'
  END SELECT
  CALL GET_COMMAND_ARGUMENT(4, CLA)
  IF (LEN_TRIM(CLA) .LE. 0) STOP 'FILE'
  CALL BFOPEN(TRIM(CLA)//'.AX', 'RO', I, J)
  IF (J .NE. 0) CALL BFOPEN(TRIM(CLA)//'.A', 'RO', I, J)
  IF (J .NE. 0) STOP 'OPEN(A)'
  ALLOCATE(A(N,N))
  READ (UNIT=I, IOSTAT=J) A
  IF (J .NE. 0) STOP 'READ(A)'
  CLOSE (UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(A)'
  ALLOCATE(V(N,N))
  ALLOCATE(WRK(N,N))
  IF (JPOS .LT. 0) THEN
     WRK(1,1) = JPOS
     JPOS = -(JPOS + 1)
  ELSE ! JPOS >= 0
     WRK(1,1) = ZERO
  END IF
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) STOP 'JPOS'
  AS = HUGE(AS)
  IF (L .EQ. 0) THEN
     CALL XJEVDC(N, A, N, V, N, JPOS, WRK, AS, INFO)
  ELSE ! deRijk
     CALL XJEVDR(N, A, N, V, N, JPOS, WRK, AS, INFO)
  END IF
  WRITE (OUTPUT_UNIT,'(I2,A,I6,A,I11,A)',ADVANCE='NO') INFO, ',', AS, ',', INT(WRK(1,1), INT64), ','
  FLUSH(OUTPUT_UNIT)
  CALL BFOPEN(TRIM(CLA)//'.V', 'WO', I, J)
  IF (J .NE. 0) STOP 'OPEN(V)'
  WRITE (UNIT=I, IOSTAT=J) V
  IF (J .NE. 0) STOP 'WRITE(V)'
  CLOSE (UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(V)'
  ALLOCATE(X(N,N))
  ALLOCATE(Y(N,N))
  ALLOCATE(Z(N,N))
  INFO = -AS
  DO J = 1, N
     DO I = 1, J-1
        A(I,J) = ZERO
     END DO
     Z(J,J) = A(J,J)
     A(J,J) = SCALE(A(J,J), INFO)
     Z(J,J) = SCALE(Z(J,J), INFO)
     DO I = J+1, N
        A(I,J) = ZERO
     END DO
  END DO
  ! V^-T = WRK := J V J
  DO J = 1, JPOS
     DO I = 1, JPOS
        WRK(I,J) = V(I,J)
     END DO
     DO I = JPOS+1, N
        WRK(I,J) = -V(I,J)
     END DO
  END DO
  DO J = JPOS+1, N
     DO I = 1, JPOS
        WRK(I,J) = -V(I,J)
     END DO
     DO I = JPOS+1, N
        WRK(I,J) = V(I,J)
     END DO
  END DO
  ! Y := V^-T D
  DO J = 1, N
     DO I = 1, N
        Y(I,J) = WRK(I,J) * Z(J,J)
     END DO
  END DO
  ! Z := V^-1 = (V^-T)^T = WRK^T
  DO J = 1, N
     DO I = 1, N
        Z(I,J) = WRK(J,I)
     END DO
  END DO
  DO J = 1, N
     DO I = 1, N
        X(I,J) = XZERO
     END DO
  END DO
  ! V^T A V = D ==> A = V^-T D V^-1
  CALL QMMMSQ(N, N, Y, N, Z, N, X, N)
  CALL BFOPEN(TRIM(CLA)//'.AX', 'RO', I, J)
  IF (J .NE. 0) CALL BFOPEN(TRIM(CLA)//'.A', 'RO', I, J)
  IF (J .NE. 0) STOP 'OPEN(A)'
  READ (UNIT=I, IOSTAT=J) A
  IF (J .NE. 0) STOP 'READ(A)'
  CLOSE (UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(A)'
  W = XZERO
  DO J = 1, N
     DO I = 1, N
        Y(I,J) = A(I,J) - X(I,J)
        W = HYPOTX(W, Y(I,J))
     END DO
  END DO
  IF (W .NE. XZERO) THEN
     M = XZERO
     DO J = 1, N
        DO I = 1, N
           M = HYPOTX(M, REAL(A(I,J), KK))
        END DO
     END DO
     W = W / M
  END IF
#ifdef __GFORTRAN__
  WRITE (OUTPUT_UNIT,'(ES30.21E4)') W
#else
  WRITE (OUTPUT_UNIT,'(ES45.36E4)') W
#endif
  DEALLOCATE(Z)
  DEALLOCATE(Y)
  DEALLOCATE(X)
  DEALLOCATE(WRK)
  DEALLOCATE(V)
  DEALLOCATE(A)
END PROGRAM XJEVDT

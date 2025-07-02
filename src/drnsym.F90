PROGRAM DRNSYM
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, ERROR_UNIT, REAL64
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, ERROR_UNIT, REAL64, REAL128
#endif
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  IMPLICIT NONE
#include "cr.f90"
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: KK = c_long_double
#else
  INTEGER, PARAMETER :: KK = REAL128
#endif
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=KK), PARAMETER :: ZERO = 0.0_KK
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=KK) :: X, Y, Z
  INTEGER :: I, J, L, N
  INTEGER, ALLOCATABLE :: ISEED(:)
  REAL(KIND=K), ALLOCATABLE :: G(:,:), A(:,:)
  EXTERNAL :: BFOPEN
  ! random seed may be given
  CALL RANDOM_SEED(SIZE=J)
  IF (J .LE. 0) STOP 'seed size non-positive'
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .LT. 1) THEN
     IF (J .GT. 1) THEN
        WRITE (CLA,'(I1)') J
        CLA = 'args: N [SEED1 ... SEED'//TRIM(CLA)//']'
     ELSE ! J = 1
        CLA = 'args: N [SEED1]'
     END IF
     WRITE (ERROR_UNIT,*) TRIM(CLA)
     STOP 'All SEED arguments have to be given, or none of them.'
  END IF
  IF (I .EQ. 1) THEN
     ALLOCATE(ISEED(J))
     CALL RANDOM_SEED
     CALL RANDOM_SEED(GET=ISEED)
  ELSE IF (I .EQ. (J + 1)) THEN
     ALLOCATE(ISEED(J))
     DO I = 1, J
        CALL GET_COMMAND_ARGUMENT(I + 1, CLA)
        READ (CLA,*) ISEED(I)
     END DO
     CALL RANDOM_SEED(PUT=ISEED)
  ELSE ! a wrong SEED
     STOP 'invalid number of SEED arguments'
  END IF
  WRITE (OUTPUT_UNIT,'(I11)',ADVANCE='NO') ISEED(1)
  DO I = 2, J-1
     WRITE (OUTPUT_UNIT,'(I12)',ADVANCE='NO') ISEED(I)
  END DO
  WRITE (OUTPUT_UNIT,'(I12)') ISEED(J)
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) N
  IF (N .LE. 0) STOP 'N <= 0'
  ALLOCATE(G(N,N))
  CALL RANDOM_NUMBER(G)
  CALL BFOPEN(TRIM(CLA)//'.Y', 'RO', I, J)
  IF (J .NE. 0) STOP 'BFOPEN(Y)'
  READ (UNIT=I,IOSTAT=J) G
  IF (J .NE. 0) STOP 'READ(Y)'
  CLOSE(UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(Y)'
  ALLOCATE(A(N,N))
  DO J = 1, N
     DO I = J, N
        Z = ZERO
        DO L = 1, N
           X = G(L,I)
           Y = G(L,J)
           Z = XFMA(X, Y, Z)
        END DO
        A(I,J) = REAL(Z, K)
     END DO
  END DO
  DO J = 2, N
     DO I = 1, J-1
        A(I,J) = A(J,I)
     END DO
  END DO
  CALL BFOPEN(TRIM(CLA)//'.A', 'WO', I, J)
  IF (J .NE. 0) STOP 'BFOPEN(A)'
  WRITE (UNIT=I,IOSTAT=J) A
  IF (J .NE. 0) STOP 'WRITE(A)'
  CLOSE(UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(A)'
  DEALLOCATE(A)
  DEALLOCATE(G)
  DEALLOCATE(ISEED)
END PROGRAM DRNSYM

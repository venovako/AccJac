PROGRAM RANHER
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, REAL64
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, REAL64, REAL128
#endif
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION WFMA(A, B, C)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(IN) :: A, B, C
       COMPLEX(KIND=c_long_double) :: WFMA
#else
       COMPLEX(KIND=REAL128), INTENT(IN) :: A, B, C
       COMPLEX(KIND=REAL128) :: WFMA
#endif
     END FUNCTION WFMA
  END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: KK = c_long_double
#else
  INTEGER, PARAMETER :: KK = REAL128
#endif
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=KK), PARAMETER :: XZERO = 0.0_KK
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  CHARACTER(LEN=256) :: CLA
  COMPLEX(KIND=KK) :: X, Y, Z
  INTEGER :: I, J, L, N
  INTEGER, ALLOCATABLE :: ISEED(:)
  COMPLEX(KIND=K), ALLOCATABLE :: G(:,:), A(:,:)
  EXTERNAL :: BFOPEN, ZRN
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
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) N
  IF (N .LE. 0) STOP 'N <= 0'
  ALLOCATE(G(N,N))
  CALL ZRN(N, N, G)
  CALL BFOPEN(TRIM(CLA)//'.Y', 'WO', I, J)
  IF (J .NE. 0) STOP 'BFOPEN(Y)'
  WRITE (UNIT=I,IOSTAT=J) G
  IF (J .NE. 0) STOP 'WRITE(Y)'
  CLOSE(UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(Y)'
  ALLOCATE(A(N,N))
  DO J = 1, N
     DO I = J, N
        Z = XZERO
        DO L = 1, N
           X = CONJG(G(L,I))
           Y = G(L,J)
           Z = WFMA(X, Y, Z)
        END DO
        A(I,J) = CMPLX(REAL(REAL(Z), K), REAL(AIMAG(Z), K), K)
     END DO
  END DO
  DO J = 1, N
     DO I = 1, J-1
        A(I,J) = CONJG(A(J,I))
     END DO
     A(J,J) = CMPLX(REAL(A(J,J)), ZERO, K)
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
END PROGRAM RANHER

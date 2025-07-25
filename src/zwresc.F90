PROGRAM ZWRESC
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
#endif
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL64
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: KK = c_long_double
#else
  INTEGER, PARAMETER :: KK = REAL128
#endif
  CHARACTER(LEN=256) :: CLA
  INTEGER :: M, N, J, L, U, I, O, Q
  COMPLEX(KIND=K), ALLOCATABLE :: G(:,:)
  COMPLEX(KIND=KK), ALLOCATABLE :: GG(:,:)
  EXTERNAL :: BFOPEN, WBWR1
  CALL GET_COMMAND_ARGUMENT(0, CLA)
  IF (COMMAND_ARGUMENT_COUNT() .NE. 6) STOP TRIM(CLA)//' M N J L U FN'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) M
  IF (M .LE. 0) STOP 'M'
  CALL GET_COMMAND_ARGUMENT(2, CLA)
  READ (CLA,*) N
  IF ((N .LE. 0) .OR. (N .GT. M)) STOP 'N'
  CALL GET_COMMAND_ARGUMENT(3, CLA)
  READ (CLA,*) J
  IF ((J .LT. 0) .OR. (J .GT. N)) STOP 'J'
  CALL GET_COMMAND_ARGUMENT(4, CLA)
  READ (CLA,*) L
  CALL GET_COMMAND_ARGUMENT(5, CLA)
  READ (CLA,*) U
  CALL GET_COMMAND_ARGUMENT(6, CLA)
  ALLOCATE(G(M,N))
  CALL BFOPEN(TRIM(CLA)//'.Y', 'RO', I, O)
  IF (O .NE. 0) STOP 'OPEN(Y)'
  READ (UNIT=I, IOSTAT=O) G
  IF (O .NE. 0) STOP 'READ(Y)'
  CLOSE (UNIT=I, IOSTAT=O)
  IF (O .NE. 0) STOP 'CLOSE(Y)'
  ALLOCATE(GG(M,N))
  DO O = 1, J
     Q = L + (O - 1) * U
     DO I = 1, M
        GG(I,O) = CMPLX(REAL(REAL(G(I,O)), KK), REAL(AIMAG(G(I,O)), KK), KK)
        GG(I,O) = CMPLX(SCALE(REAL(GG(I,O)), Q), SCALE(AIMAG(GG(I,O)), Q), KK)
     END DO
  END DO
  DO O = J+1, N
     Q = L + (O - 1) * U
     DO I = 1, M
        GG(I,O) = CMPLX(REAL(REAL(G(I,O)), KK), REAL(AIMAG(G(I,O)), KK), KK)
        GG(I,O) = CMPLX(SCALE(REAL(GG(I,O)), Q), SCALE(AIMAG(GG(I,O)), Q), KK)
     END DO
  END DO
  CALL BFOPEN(TRIM(CLA)//'.YX', 'WO', I, O)
  IF (O .NE. 0) STOP 'OPEN(YX)'
  CALL WBWR1(I, M * N, GG, O)
  IF (O .NE. 0) STOP 'WRITE(YX)'
  CLOSE (UNIT=I, IOSTAT=O)
  IF (O .NE. 0) STOP 'CLOSE(YX)'
  DO O = 1, N
     DO I = 1, M
        G(I,O) = CMPLX(REAL(REAL(GG(I,O)), K), REAL(AIMAG(G(I,O)), K), K)
     END DO
  END DO
  DEALLOCATE(GG)
  CALL BFOPEN(TRIM(CLA)//'.X', 'WO', I, O)
  IF (O .NE. 0) STOP 'OPEN(X)'
  WRITE (UNIT=I, IOSTAT=O) G
  IF (O .NE. 0) STOP 'WRITE(X)'
  CLOSE (UNIT=I, IOSTAT=O)
  IF (O .NE. 0) STOP 'CLOSE(X)'
  DEALLOCATE(G)
END PROGRAM ZWRESC

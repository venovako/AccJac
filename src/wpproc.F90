PROGRAM WPPROC
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#endif
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, REAL128
  IMPLICIT NONE
#include "cr.f90"
  INTEGER, PARAMETER :: KK = REAL128
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  REAL(KIND=KK), PARAMETER :: XZERO = 0.0_KK, SQRT2 = SQRT(2.0_KK)
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  CHARACTER(LEN=11) :: FN
  CHARACTER :: T
  COMPLEX(KIND=KK) :: Z
  REAL(KIND=KK) :: X, Y, D
  INTEGER :: I, J, L, N, S, U, V
  COMPLEX(KIND=K), ALLOCATABLE :: W1(:,:), A1(:,:), W3(:,:), A3(:,:)
  REAL(KIND=K), ALLOCATABLE :: PR(:,:)
  ! read the command line arguments
  L = COMMAND_ARGUMENT_COUNT()
  IF (L .NE. 2) STOP 'wpproc.exe N S'
  CALL GET_COMMAND_ARGUMENT(1, FN)
  READ (FN,*) N
  IF (N .LT. 0) THEN
     N = -N
     T = 'W'
  ELSE IF (N .GT. 0) THEN
     T = 'w'
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
  ALLOCATE(PR(N,N))
  DO J = 1, N
     PR(J,J) = ZERO
  END DO
  WRITE (OUTPUT_UNIT,'(A)') '"SWEEP", "maxRE(eTAN[H])", "maxRE|eTAN[H]|", "maxRE(tan)", "maxRE(A)"'
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
        DO I = 1, J-1
           A1(I,J) = W1(I,J)
           X = REAL(W1(J,I))
           Y = AIMAG(W1(J,I))
           D = CR_HYPOT(X, Y)
           IF (Y .NE. XZERO) Y = Y / X
           W1(I,J) = CMPLX(REAL(D, K), REAL(Y, K), K)
        END DO
        A1(J,J) = REAL(W1(J,J))
        W1(J,J) = ZERO
     END DO
     DO J = 2, N
        DO I = 1, J-1
           PR(I,J) = AIMAG(W1(I,J))
           PR(J,I) = REAL(W1(I,J))
        END DO
     END DO
     WRITE (FN,'(A,I3.3,A,I2.2,A)') T, N, 's', L, '.txt'
     OPEN(NEWUNIT=U, IOSTAT=V, FILE=FN, STATUS='REPLACE', ACTION='WRITE', ACCESS='SEQUENTIAL', FORM='FORMATTED')
     IF (V .NE. 0) STOP 'OPEN(s)'
     IF (N .EQ. 1) THEN
#ifdef __GFORTRAN__
        WRITE (U,'(ES30.21E4)') PR(1,1)
#else
        WRITE (U,'(ES45.36E4)') PR(1,1)
#endif
     ELSE ! N > 1
        DO I = 1, N
           DO J = 1, N-1
              IF (J .EQ. 1) THEN
#ifdef __GFORTRAN__
                 WRITE (U,'(ES30.21E4)',ADVANCE='NO') PR(I,1)
#else
                 WRITE (U,'(ES45.36E4)',ADVANCE='NO') PR(I,1)
#endif
              ELSE ! J > 1
#ifdef __GFORTRAN__
                 WRITE (U,'(ES31.21E4)',ADVANCE='NO') PR(I,J)
#else
                 WRITE (U,'(ES46.36E4)',ADVANCE='NO') PR(I,J)
#endif
              END IF
           END DO
#ifdef __GFORTRAN__
           WRITE (U,'(ES31.21E4)') PR(I,N)
#else
           WRITE (U,'(ES46.36E4)') PR(I,N)
#endif
        END DO
     END IF
     CLOSE(UNIT=U, IOSTAT=V)
     IF (V .NE. 0) STOP 'CLOSE(s)'
     DO J = 1, N-1
        DO I = J+1, N
           A1(I,J) = CONJG(A1(J,I))
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
        DO I = 1, J-1
           A3(I,J) = W3(I,J)
           X = REAL(W3(J,I))
           Y = AIMAG(W3(J,I))
           D = CR_HYPOT(X, Y)
           IF (Y .NE. XZERO) Y = Y / X
           W3(I,J) = CMPLX(REAL(D, K), REAL(Y, K), K)
        END DO
        A3(J,J) = REAL(W3(J,J))
        W3(J,J) = ZERO
     END DO
     DO J = 2, N
        DO I = 1, J-1
           PR(I,J) = AIMAG(W3(I,J))
           PR(J,I) = REAL(W3(I,J))
        END DO
     END DO
     WRITE (FN,'(A,I3.3,A,I2.2,A)') T, N, 'm', L, '.txt'
     OPEN(NEWUNIT=U, IOSTAT=V, FILE=FN, STATUS='REPLACE', ACTION='WRITE', ACCESS='SEQUENTIAL', FORM='FORMATTED')
     IF (V .NE. 0) STOP 'OPEN(m)'
     IF (N .EQ. 1) THEN
#ifdef __GFORTRAN__
        WRITE (U,'(ES30.21E4)') PR(1,1)
#else
        WRITE (U,'(ES45.36E4)') PR(1,1)
#endif
     ELSE ! N > 1
        DO I = 1, N
           DO J = 1, N-1
              IF (J .EQ. 1) THEN
#ifdef __GFORTRAN__
                 WRITE (U,'(ES30.21E4)',ADVANCE='NO') PR(I,1)
#else
                 WRITE (U,'(ES45.36E4)',ADVANCE='NO') PR(I,1)
#endif
              ELSE ! J > 1
#ifdef __GFORTRAN__
                 WRITE (U,'(ES31.21E4)',ADVANCE='NO') PR(I,J)
#else
                 WRITE (U,'(ES46.36E4)',ADVANCE='NO') PR(I,J)
#endif
              END IF
           END DO
#ifdef __GFORTRAN__
           WRITE (U,'(ES31.21E4)') PR(I,N)
#else
           WRITE (U,'(ES46.36E4)') PR(I,N)
#endif
        END DO
     END IF
     CLOSE(UNIT=U, IOSTAT=V)
     IF (V .NE. 0) STOP 'CLOSE(m)'
     DO J = 1, N-1
        DO I = J+1, N
           A3(I,J) = CONJG(A3(J,I))
        END DO
     END DO
     X = XZERO
     DO J = 1, N-1
        DO I = J+1, N
           Z = CMPLX(REAL(W1(I,J)), AIMAG(W1(I,J)), KK) - CMPLX(REAL(W3(I,J)), AIMAG(W3(I,J)), KK)
           Y = CR_HYPOT(REAL(Z), AIMAG(Z))
           IF (Y .NE. XZERO) Y = Y / CR_HYPOT(REAL(REAL(W1(I,J)), KK), REAL(AIMAG(W1(I,J)), KK))
           IF (Y .GT. X) X = Y
        END DO
     END DO
#ifdef __GFORTRAN__
     WRITE (OUTPUT_UNIT,'(I2,A,ES30.21E4,A)',ADVANCE='NO') L, ',', X, ','
#else
     WRITE (OUTPUT_UNIT,'(I2,A,ES45.36E4,A)',ADVANCE='NO') L, ',', X, ','
#endif
     FLUSH(OUTPUT_UNIT)
     X = XZERO
     DO J = 2, N
        DO I = 1, J-1
           Y = REAL(REAL(W1(I,J)), KK) - REAL(REAL(W3(I,J)), KK)
           IF (Y .NE. XZERO) Y = ABS(Y / REAL(REAL(W1(I,J)), KK))
           IF (Y .GT. X) X = Y
        END DO
     END DO
#ifdef __GFORTRAN__
     WRITE (OUTPUT_UNIT,'(ES30.21E4,A)',ADVANCE='NO') X, ','
#else
     WRITE (OUTPUT_UNIT,'(ES45.36E4,A)',ADVANCE='NO') X, ','
#endif
     FLUSH(OUTPUT_UNIT)
     X = XZERO
     DO J = 2, N
        DO I = 1, J-1
           Y = REAL(AIMAG(W1(I,J)), KK) - REAL(AIMAG(W3(I,J)), KK)
           IF (Y .NE. XZERO) Y = ABS(Y / REAL(AIMAG(W1(I,J)), KK))
           IF (Y .GT. X) X = Y
        END DO
     END DO
#ifdef __GFORTRAN__
     WRITE (OUTPUT_UNIT,'(ES30.21E4,A)',ADVANCE='NO') X, ','
#else
     WRITE (OUTPUT_UNIT,'(ES45.36E4,A)',ADVANCE='NO') X, ','
#endif
     FLUSH(OUTPUT_UNIT)
     X = XZERO
     DO J = 1, N
        DO I = 1, N
           Z = CMPLX(REAL(A1(I,J)), AIMAG(A1(I,J)), KK) - CMPLX(REAL(A3(I,J)), AIMAG(A3(I,J)), KK)
           Y = CR_HYPOT(REAL(Z), AIMAG(Z))
           X = CR_HYPOT(X, Y)
        END DO
     END DO
     IF (X .NE. XZERO) THEN
        Y = XZERO
        DO J = 2, N
           DO I = 1, J-1
              Y = CR_HYPOT(Y, CR_HYPOT(REAL(REAL(A1(I,J)), KK), REAL(AIMAG(A1(I,J)), KK)))
           END DO
        END DO
        Y = Y * SQRT2
        DO J = 1, N
           Y = CR_HYPOT(Y, REAL(REAL(A1(J,J)), KK))
        END DO
        X = X / Y
     END IF
#ifdef __GFORTRAN__
     WRITE (OUTPUT_UNIT,'(ES30.21E4)') X
#else
     WRITE (OUTPUT_UNIT,'(ES45.36E4)') X
#endif
  END DO
  DEALLOCATE(PR)
  DEALLOCATE(A3)
  DEALLOCATE(W3)
  DEALLOCATE(A1)
  DEALLOCATE(W1)
END PROGRAM WPPROC

PROGRAM ZJEVDT
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, OUTPUT_UNIT, REAL64
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, OUTPUT_UNIT, REAL64, REAL128
#endif
  IMPLICIT NONE
  INTERFACE
     SUBROUTINE JSWEEP(J, N, S, P, O, INFO)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: J, N
       INTEGER, INTENT(OUT) :: S, P, O(2,*), INFO
     END SUBROUTINE JSWEEP
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE WMMMSQ(N, A, LDA, B, LDB, C, LDC)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDA, LDB, LDC
#ifdef __GFORTRAN__
       COMPLEX(KIND=c_long_double), INTENT(IN) :: A(LDA,N), B(LDB,N)
       COMPLEX(KIND=c_long_double), INTENT(OUT) :: C(LDC,N)
#else
       COMPLEX(KIND=REAL128), INTENT(IN) :: A(LDA,N), B(LDB,N)
       COMPLEX(KIND=REAL128), INTENT(OUT) :: C(LDC,N)
#endif
     END SUBROUTINE WMMMSQ
  END INTERFACE
#ifdef __GFORTRAN__
  INTERFACE
     PURE FUNCTION HYPOTX(X, Y) BIND(C,NAME='cr_hypotl')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_long_double) :: HYPOTX
     END FUNCTION HYPOTX
  END INTERFACE
  INTEGER, PARAMETER :: KK = c_long_double
#else
#define HYPOTX HYPOT
  INTEGER, PARAMETER :: KK = REAL128
#endif
  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=KK), PARAMETER :: XZERO = 0.0_KK
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=KK) :: W, M
  INTEGER :: N, JPOS, INFO, I, J, L, O, AS
  COMPLEX(KIND=K), ALLOCATABLE :: A(:,:), V(:,:), WRK(:,:)
  COMPLEX(KIND=KK), ALLOCATABLE :: X(:,:), Y(:,:), Z(:,:)
  INTEGER, ALLOCATABLE :: ORD(:,:)
  EXTERNAL :: BFOPEN, ZJEVDC, ZJEVDR, ZJEVDM
  ! read the command line arguments
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .NE. 4) STOP 'zjevdt.exe N JPOS OPTS FILE'
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
  CASE (8,9)
     L = 5
  CASE (10,11)
     L = 7
  CASE DEFAULT
     STOP 'OPTS'
  END SELECT
  CALL GET_COMMAND_ARGUMENT(4, CLA)
  IF (LEN_TRIM(CLA) .LE. 0) STOP 'FILE'
  CALL BFOPEN(TRIM(CLA)//'.A', 'RO', I, J)
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
     CALL ZJEVDC(N, A, N, V, N, JPOS, WRK, AS, INFO)
  ELSE IF (L .EQ. 1) THEN
     CALL ZJEVDR(N, A, N, V, N, JPOS, WRK, AS, INFO)
  ELSE ! parallel
     J = N / 2
     IF (L .EQ. 5) THEN
        I = N - 1
     ELSE ! modified modulus
        I = N
     END IF
     ALLOCATE(ORD(2,J*(1+I)))
     ORD = 0
     O = 1 + J
     CALL JSWEEP(L, N, I, J, ORD(1,O), O)
     IF (O .NE. 0) STOP 'JSWEEP'
     CALL ZJEVDM(N, A, N, V, N, JPOS, WRK, AS, ORD, INFO)
     DEALLOCATE(ORD)
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
     A(J,J) = SCALE(REAL(A(J,J)), INFO)
     Z(J,J) = SCALE(REAL(Z(J,J)), INFO)
     DO I = J+1, N
        A(I,J) = ZERO
     END DO
  END DO
  ! V^-H = WRK := J V J
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
  ! Y := V^-H D
  DO J = 1, N
     DO I = 1, N
        Y(I,J) = WRK(I,J) * Z(J,J)
     END DO
  END DO
  ! Z := V^-1 = (V^-H)^H = WRK^H
  DO J = 1, N
     DO I = 1, N
        Z(I,J) = CONJG(WRK(J,I))
     END DO
  END DO
  ! V^H A V = D ==> A = V^-H D V^-1
  CALL WMMMSQ(N, Y, N, Z, N, X, N)
  CALL BFOPEN(TRIM(CLA)//'.A', 'RO', I, J)
  IF (J .NE. 0) STOP 'OPEN(A)'
  READ (UNIT=I, IOSTAT=J) A
  IF (J .NE. 0) STOP 'READ(A)'
  CLOSE (UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(A)'
  W = XZERO
  DO J = 1, N
     DO I = 1, N
        Y(I,J) = A(I,J) - X(I,J)
        W = HYPOTX(W, HYPOTX(REAL(Y(I,J)), AIMAG(Y(I,J))))
     END DO
  END DO
  IF (W .NE. XZERO) THEN
     M = XZERO
     DO J = 1, N
        DO I = 1, N
           M = HYPOTX(M, HYPOTX(REAL(REAL(A(I,J)), KK), REAL(AIMAG(A(I,J)), KK)))
        END DO
     END DO
     W = W / M
  END IF
  WRITE (OUTPUT_UNIT,'(ES25.17E3)') W
  DEALLOCATE(Z)
  DEALLOCATE(Y)
  DEALLOCATE(X)
  DEALLOCATE(WRK)
  DEALLOCATE(V)
  DEALLOCATE(A)
END PROGRAM ZJEVDT

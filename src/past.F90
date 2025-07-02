PROGRAM PAST
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, OUTPUT_UNIT, REAL64
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, OUTPUT_UNIT, REAL64, REAL128
#endif
  IMPLICIT NONE
#include "cr.f90"
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: KK = c_long_double
#else
  INTEGER, PARAMETER :: KK = REAL128
#endif
  REAL(KIND=REAL64), PARAMETER :: DPI = 3.14159265358979323846_REAL64
  CHARACTER(LEN=256) :: CLA
  INTEGER(KIND=INT64) :: K
  INTEGER :: N, I, J, R
  LOGICAL :: C
  INTEGER(KIND=INT64), ALLOCATABLE :: L(:,:), U(:,:), S(:,:)
  REAL(KIND=REAL64), ALLOCATABLE :: A(:,:), RU(:,:)
  REAL(KIND=KK), ALLOCATABLE :: AA(:,:)
  COMPLEX(KIND=REAL64), ALLOCATABLE :: Z(:,:)
  COMPLEX(KIND=KK), ALLOCATABLE :: ZZ(:,:)
  EXTERNAL :: PASCAL, WBWR1, XBWR1
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .NE. 2) STOP 'past.exe N FN'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) N
  IF (N .LT. 0) THEN
     N = -N
     C = .TRUE.
  ELSE IF (N .GT. 0) THEN
     C = .FALSE.
  ELSE ! N = 0
     STOP 'N = 0'
  END IF
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
  IF (C) THEN
     ! roots of unity
     ALLOCATE(RU(3,N))
     DO J = 1, N
        RU(1,J) = ((2 * (J - 1)) * DPI) / N
        CALL CR_SINCOS(RU(1,J), RU(3,J), RU(2,J))
     END DO
  END IF
  IF (C) THEN
     ALLOCATE(Z(N,N))
     ALLOCATE(ZZ(N,N))
     DO J = 1, N-1
        Z(J,J) = L(J,J)
        ZZ(J,J) = L(J,J)
        DO I = J+1, N
           Z(I,J) = CMPLX((RU(2,I) * L(I,J)), (RU(3,I) * L(I,J)), REAL64)
           ZZ(I,J) = CMPLX((RU(2,I) * L(I,J)), (RU(3,I) * L(I,J)), KK)
        END DO
     END DO
     DO J = 2, N
        DO I = 1, J-1
           Z(I,J) = 0.0_REAL64
           ZZ(I,J) = 0.0_KK
        END DO
     END DO
     Z(N,N) = L(N,N)
     ZZ(N,N) = L(N,N)
  ELSE ! real
     ALLOCATE(A(N,N))
     ALLOCATE(AA(N,N))
     DO J = 1, N
        DO I = 1, N
           A(I,J) = L(I,J)
           AA(I,J) = L(I,J)
        END DO
     END DO
  END IF
  CALL BFOPEN(TRIM(CLA)//'.Y', 'WO', I, J)
  IF (J .NE. 0) STOP 'BFOPEN(Y)'
  IF (C) THEN
     WRITE (UNIT=I,IOSTAT=J) Z
  ELSE ! real
     WRITE (UNIT=I,IOSTAT=J) A
  END IF
  IF (J .NE. 0) STOP 'WRITE(Y)'
  CLOSE(UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(Y)'
  CALL BFOPEN(TRIM(CLA)//'.YX', 'WO', I, J)
  IF (J .NE. 0) STOP 'BFOPEN(YX)'
  R = N * N
  IF (C) THEN
     ! WRITE (UNIT=I,IOSTAT=J) ZZ
     CALL WBWR1(I, R, ZZ, J)
  ELSE ! real
     ! WRITE (UNIT=I,IOSTAT=J) AA
     CALL XBWR1(I, R, AA, J)
  END IF
  IF (J .NE. 0) STOP 'WRITE(YX)'
  CLOSE(UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(YX)'
  ! e^{i(2(I-1)π/N)} * e^{-i(2(J-1)π/N)} = e^{i(2(I-J)π/N)}}
  IF (C) THEN
     DO J = 1, N-1
        Z(J,J) = S(J,J)
        ZZ(J,J) = S(J,J)
        DO I = J+1, N
           R = I - J + 1
           Z(I,J) = CMPLX((RU(2,R) * S(I,J)), (RU(3,R) * S(I,J)), REAL64)
           ZZ(I,J) = CMPLX((RU(2,R) * S(I,J)), (RU(3,R) * S(I,J)), KK)
        END DO
     END DO
     DO J = 2, N
        DO I = 1, J-1
           Z(I,J) = CONJG(Z(J,I))
           ZZ(I,J) = CONJG(ZZ(J,I))
        END DO
     END DO
     Z(N,N) = S(N,N)
     ZZ(N,N) = S(N,N)
  ELSE ! real
     DO J = 1, N
        DO I = 1, N
           A(I,J) = S(I,J)
           AA(I,J) = S(I,J)
        END DO
     END DO
  END IF
  CALL BFOPEN(TRIM(CLA)//'.A', 'WO', I, J)
  IF (J .NE. 0) STOP 'BFOPEN(A)'
  IF (C) THEN
     WRITE (UNIT=I,IOSTAT=J) Z
  ELSE ! real
     WRITE (UNIT=I,IOSTAT=J) A
  END IF
  IF (J .NE. 0) STOP 'WRITE(A)'
  CLOSE(UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(A)'
  CALL BFOPEN(TRIM(CLA)//'.AX', 'WO', I, J)
  IF (J .NE. 0) STOP 'BFOPEN(AX)'
  R = N * N
  IF (C) THEN
     ! WRITE (UNIT=I,IOSTAT=J) ZZ
     CALL WBWR1(I, R, ZZ, J)
  ELSE ! real
     ! WRITE (UNIT=I,IOSTAT=J) AA
     CALL XBWR1(I, R, AA, J)
  END IF
  IF (J .NE. 0) STOP 'WRITE(AX)'
  CLOSE(UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(AX)'
  IF (C) THEN
     DEALLOCATE(ZZ)
     DEALLOCATE(Z)
     DEALLOCATE(RU)
  ELSE ! real
     DEALLOCATE(AA)
     DEALLOCATE(A)
  END IF
  DEALLOCATE(S)
  DEALLOCATE(U)
  DEALLOCATE(L)
END PROGRAM PAST

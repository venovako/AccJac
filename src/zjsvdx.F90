PROGRAM ZJSVDX
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, INT64, OUTPUT_UNIT, REAL64
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, INT64, OUTPUT_UNIT, REAL64, REAL128
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
  REAL(KIND=KK), PARAMETER :: XZERO = 0.0_KK
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=KK) :: X, Y
  COMPLEX(KIND=K) :: T
  REAL(KIND=K) :: R
  INTEGER :: M, N, LDG, LDV, JPOS, GS, INFO, I, J, L
  INTEGER(KIND=INT64), ALLOCATABLE :: CLK(:)
  COMPLEX(KIND=K), ALLOCATABLE :: G(:,:), V(:,:), WRK(:,:)
  REAL(KIND=K), ALLOCATABLE :: SV(:), LY(:)
  INTEGER, ALLOCATABLE :: IX(:)
  EXTERNAL :: BFOPEN, ZJSVDF
  ! read the command line arguments
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .NE. 5) STOP 'zjsvdx.exe M N JPOS OPTS FILE'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) M
  IF (M .LE. 0) STOP 'M'
  CALL GET_COMMAND_ARGUMENT(2, CLA)
  READ (CLA,*) N
  IF ((N .LE. 0) .OR. (N .GT. M)) STOP 'N'
  CALL GET_COMMAND_ARGUMENT(3, CLA)
  READ (CLA,*) JPOS
  IF ((JPOS .LT. -1) .OR. (JPOS .GT. N)) STOP 'JPOS'
  CALL GET_COMMAND_ARGUMENT(4, CLA)
  READ (CLA,*) L
  IF (L .LT. 0) STOP 'OPTS'
  CALL GET_COMMAND_ARGUMENT(5, CLA)
  IF (LEN_TRIM(CLA) .LE. 0) STOP 'FILE'
  ! check J
  IF (JPOS .EQ. -1) THEN
     ALLOCATE(CLK(MAX(N,3)))
     CALL BFOPEN(TRIM(CLA)//'.J', 'RO', I, J)
     IF (J .NE. 0) STOP 'OPEN(J)'
     READ (UNIT=I, IOSTAT=J) CLK
     IF (J .NE. 0) STOP 'READ(J)'
     CLOSE (UNIT=I, IOSTAT=J)
     IF (J .NE. 0) STOP 'CLOSE(J)'
     JPOS = 0
     J = 1
     DO WHILE (CLK(J) .EQ. 1_INT64)
        JPOS = JPOS + 1
        J = J + 1
        IF (J .GT. N) EXIT
     END DO
     IF (J .LE. N) THEN
        DO WHILE (CLK(J) .EQ. -1_INT64)
           J = J + 1
           IF (J .GT. N) EXIT
        END DO
        IF (J .LE. N) STOP 'J not [+I,-I]'
     END IF
  ELSE ! JPOS .NE. -1
     ALLOCATE(CLK(3))
  END IF
  ! set G
  LDG = M
  ALLOCATE(G(LDG,N))
  CALL BFOPEN(TRIM(CLA)//'.Y', 'RO', I, J)
  IF (J .NE. 0) STOP 'OPEN(Y)'
  READ (UNIT=I, IOSTAT=J) G
  IF (J .NE. 0) STOP 'READ(Y)'
  CLOSE (UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(Y)'
  ! allocate the rest
  LDV = N
  ALLOCATE(V(LDV,N))
  ALLOCATE(WRK(M,N))
  ALLOCATE(SV(N))
  ALLOCATE(LY(N))
  ALLOCATE(IX(N))
  ! call ZJSVDF
  GS = HUGE(GS)
  INFO = L
  IX(1) = ERROR_UNIT
  CALL SYSTEM_CLOCK(CLK(1), CLK(2), CLK(3))
  CALL ZJSVDF(M, N, G, LDG, V, LDV, JPOS, SV, GS, IX, WRK, LY, INFO)
  CALL SYSTEM_CLOCK(CLK(3))
  CLK(1) = CLK(3) - CLK(1)
  CLK(3) = MOD(CLK(1), CLK(2)) * 1000000_INT64
  CLK(1) = CLK(1) / CLK(2)
  CLK(3) = CLK(3) / CLK(2)
  R = LY(N)
  CLK(2) = INT(R, INT64)
  WRITE (OUTPUT_UNIT,'(I11,A,I12,A,I6,A,I8,A,I6.6,A)',ADVANCE='NO') INFO, ',', CLK(2), ',', GS, ',', CLK(1), '.', CLK(3), ','
  FLUSH(OUTPUT_UNIT)
  IF (INFO .LT. 0) STOP 'ZJSVDF'
  CALL BFOPEN(TRIM(CLA)//'.YU', 'WO', I, J)
  IF (J .NE. 0) STOP 'OPEN(YU)'
  WRITE (UNIT=I, IOSTAT=J) G
  IF (J .NE. 0) STOP 'WRITE(YU)'
  CLOSE (UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(YU)'
  CALL BFOPEN(TRIM(CLA)//'.YV', 'WO', I, J)
  IF (J .NE. 0) STOP 'OPEN(YV)'
  WRITE (UNIT=I, IOSTAT=J) V
  IF (J .NE. 0) STOP 'WRITE(YV)'
  CLOSE (UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(YV)'
  CALL BFOPEN(TRIM(CLA)//'.SS', 'WO', I, J)
  IF (J .NE. 0) STOP 'OPEN(SS)'
  WRITE (UNIT=I, IOSTAT=J) SV
  IF (J .NE. 0) STOP 'WRITE(SS)'
  CLOSE (UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(SS)'
  ! V^-1 = J V^H J
  DO J = 1, N
     DO I = 1, J-1
        T = V(I,J)
        V(I,J) = CONJG(V(J,I))
        V(J,I) = CONJG(T)
     END DO
     V(J,J) = CONJG(V(J,J))
  END DO
  DO J = 1, JPOS
     DO I = JPOS+1, N
        V(I,J) = -V(I,J)
     END DO
  END DO
  DO J = JPOS+1, N
     DO I = 1, JPOS
        V(I,J) = -V(I,J)
     END DO
  END DO
  CALL BFOPEN(TRIM(CLA)//'.ZZ', 'WO', I, J)
  IF (J .NE. 0) STOP 'OPEN(ZZ)'
  WRITE (UNIT=I, IOSTAT=J) V
  IF (J .NE. 0) STOP 'WRITE(ZZ)'
  CLOSE (UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(ZZ)'
  L = -GS
  OPEN(NEWUNIT=I, IOSTAT=J, FILE=TRIM(CLA)//'.E', STATUS='REPLACE', ACTION='WRITE', ACCESS='SEQUENTIAL', FORM='FORMATTED')
  IF (J .NE. 0) STOP 'OPEN(E)'
  DO J = 1, JPOS
     X = SV(J)
     SV(J) = SCALE(SV(J), L)
     X = SCALE(X, L)
     X = X * X
     WRITE (I,'(ES25.17E3)') X
  END DO
  DO J = JPOS+1, N
     X = SV(J)
     SV(J) = SCALE(SV(J), L)
     X = SCALE(X, L)
     X = -X * X
     WRITE (I,'(ES25.17E3)') X
  END DO
  CLOSE (UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(E)'
  CALL BFOPEN(TRIM(CLA)//'.SY', 'WO', I, J)
  IF (J .NE. 0) STOP 'OPEN(SY)'
  WRITE (UNIT=I, IOSTAT=J) SV
  IF (J .NE. 0) STOP 'WRITE(SY)'
  CLOSE (UNIT=I, IOSTAT=J)
  IF (J .NE. 0) STOP 'CLOSE(SY)'
  ! read LY
  CALL BFOPEN(TRIM(CLA)//'.LY', 'RO', I, J)
  IF (J .EQ. 0) THEN
     READ (UNIT=I, IOSTAT=J) LY
     IF (J .NE. 0) STOP 'READ(LY)'
     CLOSE (UNIT=I, IOSTAT=J)
     IF (J .NE. 0) STOP 'CLOSE(LY)'
     L = 1
     I = 1
     DO WHILE (L .GT. 0)
        L = 0
        DO J = 1, N-I
           IF (LY(J) .LT. LY(J+1)) THEN
              R = LY(J)
              LY(J) = LY(J+1)
              LY(J+1) = R
              L = L + 1
           END IF
        END DO
        I = I + 1
     END DO
     L = 1
     I = 1
     DO WHILE (L .GT. 0)
        L = 0
        DO J = 1, JPOS-I
           IF (SV(J) .LT. SV(J+1)) THEN
              R = SV(J)
              SV(J) = SV(J+1)
              SV(J+1) = R
              L = L + 1
           END IF
        END DO
        I = I + 1
     END DO
     L = 1
     I = 1
     DO WHILE (L .GT. 0)
        L = 0
        DO J = JPOS+1, N-I
           IF (SV(J) .GT. SV(J+1)) THEN
              R = SV(J)
              SV(J) = SV(J+1)
              SV(J+1) = R
              L = L + 1
           END IF
        END DO
        I = I + 1
     END DO
     X = XZERO
     DO J = 1, JPOS
        Y = SV(J)
        Y = ABS(XFMA(Y, Y, REAL(-LY(J), KK)) / LY(J))
        X = MAX(X, Y)
     END DO
     DO J = JPOS+1, N
        Y = SV(J)
        Y = ABS(XFMA(Y, Y, REAL(LY(J), KK)) / LY(J))
        X = MAX(X, Y)
     END DO
  ELSE ! no LY
     X = XZERO
  END IF
  WRITE (OUTPUT_UNIT,'(ES25.17E3)') REAL(X, K)
  FLUSH(OUTPUT_UNIT)
  IF (ALLOCATED(IX)) DEALLOCATE(IX)
  IF (ALLOCATED(LY)) DEALLOCATE(LY)
  IF (ALLOCATED(SV)) DEALLOCATE(SV)
  IF (ALLOCATED(WRK)) DEALLOCATE(WRK)
  IF (ALLOCATED(V)) DEALLOCATE(V)
  IF (ALLOCATED(G)) DEALLOCATE(G)
  IF (ALLOCATED(CLK)) DEALLOCATE(CLK)
END PROGRAM ZJSVDX

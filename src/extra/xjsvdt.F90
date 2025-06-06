PROGRAM XJSVDT
#ifdef __GFORTRAN__
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long, c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, OUTPUT_UNIT, REAL128
#endif
  IMPLICIT NONE
#ifdef __GFORTRAN__
  REAL(KIND=REAL128), PARAMETER :: QZERO = 0.0_REAL128, QONE = 1.0_REAL128
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=REAL128) :: Y, Z
  REAL(KIND=c_long_double) :: T
  INTEGER(KIND=INT64) :: CLK(3)
  INTEGER :: M, N, LDG, LDV, JPOS, GS, INFO, I, J, L
  REAL(KIND=c_long_double), ALLOCATABLE :: G(:,:), V(:,:), WRK(:,:), SV(:)
  REAL(KIND=REAL128), ALLOCATABLE :: U(:,:), W(:,:)
  EXTERNAL :: BFOPEN, XJSVDC, XJSVDR
  ! read the command line arguments
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .NE. 5) STOP 'xjsvdt.exe M N JPOS OPTS FILE'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) M
  IF (M .LT. 0) THEN
     M = -M
     Z = QONE
  ELSE IF (M .GT. 0) THEN
     Z = QZERO
  ELSE ! M = 0
     STOP 'M'
  END IF
  CALL GET_COMMAND_ARGUMENT(2, CLA)
  READ (CLA,*) N
  IF ((N .LE. 0) .OR. (N .GT. M)) STOP 'N'
  CALL GET_COMMAND_ARGUMENT(3, CLA)
  READ (CLA,*) JPOS
  IF ((JPOS .LT. 0) .OR. (JPOS .GT. N)) STOP 'JPOS'
  CALL GET_COMMAND_ARGUMENT(4, CLA)
  READ (CLA,*) L
  IF (L .LT. 0) STOP 'OPTS'
  CALL GET_COMMAND_ARGUMENT(5, CLA)
  IF (LEN_TRIM(CLA) .LE. 0) STOP 'FILE'
  ! set G
  LDG = M
  ALLOCATE(G(LDG,N))
  CALL BFOPEN(TRIM(CLA)//'.YX', 'RO', I, J)
  IF (J .NE. 0) CALL BFOPEN(TRIM(CLA)//'.Y', 'RO', I, J)
  IF (J .NE. 0) STOP 'Y'
  READ (UNIT=I, IOSTAT=J) G
  IF (J .NE. 0) STOP 'G'
  CLOSE(I)
  ! allocate the rest
  LDV = N
  ALLOCATE(V(LDV,N))
  ALLOCATE(WRK(M,N))
  ALLOCATE(SV(N))
  ! call XJSVDC
  GS = HUGE(GS)
  INFO = IAND(L, 3)
  CALL SYSTEM_CLOCK(CLK(1), CLK(2), CLK(3))
  IF (IAND(L, 4) .EQ. 0) THEN
     CALL XJSVDC(M, N, G, LDG, V, LDV, JPOS, SV, WRK, GS, INFO)
  ELSE ! XJSVDR
     CALL XJSVDR(M, N, G, LDG, V, LDV, JPOS, SV, WRK, GS, INFO)
  END IF
  CALL SYSTEM_CLOCK(CLK(3))
  CLK(1) = CLK(3) - CLK(1)
  CLK(3) = MOD(CLK(1), CLK(2)) * 1000000_INT64
  CLK(1) = CLK(1) / CLK(2)
  CLK(3) = CLK(3) / CLK(2)
  WRITE (OUTPUT_UNIT,'(I11,A,I6,A,I8,A,I6.6,A)',ADVANCE='NO') INFO, ',', GS, ',', CLK(1), '.', CLK(3), ','
  FLUSH(OUTPUT_UNIT)
  IF (INFO .LT. 0) THEN
     IF (IAND(L, 4) .EQ. 0) THEN
        STOP 'XJSVDC'
     ELSE ! XJSVDR
        STOP 'XJSVDR'
     END IF
  END IF
  CALL BFOPEN(TRIM(CLA)//'.YU', 'WO', I, J)
  IF (J .NE. 0) STOP 'YU'
  WRITE (UNIT=I, IOSTAT=J) G
  IF (J .NE. 0) STOP 'U'
  CLOSE(I)
  CALL BFOPEN(TRIM(CLA)//'.YV', 'WO', I, J)
  IF (J .NE. 0) STOP 'YV'
  WRITE (UNIT=I, IOSTAT=J) V
  IF (J .NE. 0) STOP 'V'
  CLOSE(I)
  CALL BFOPEN(TRIM(CLA)//'.SS', 'WO', I, J)
  IF (J .NE. 0) STOP 'SS'
  WRITE (UNIT=I, IOSTAT=J) SV
  IF (J .NE. 0) STOP 'SV'
  CLOSE(I)
  ! V^-1 = J V^T J
  DO J = 2, N
     DO I = 1, J-1
        T = V(I,J)
        V(I,J) = V(J,I)
        V(J,I) = T
     END DO
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
  IF (J .NE. 0) STOP 'ZZ'
  WRITE (UNIT=I, IOSTAT=J) V
  IF (J .NE. 0) STOP 'Z'
  CLOSE(I)
  L = -GS
  IF (Z .EQ. QZERO) THEN
     ALLOCATE(U(M,N))
     DO J = 1, N
        Y = SV(J)
        Y = SCALE(Y, L)
        DO I = 1, M
           U(I,J) = REAL(G(I,J), REAL128) * Y
        END DO
     END DO
  END IF
  DO J = 1, N
     SV(J) = SCALE(SV(J), L)
  END DO
  CALL BFOPEN(TRIM(CLA)//'.SY', 'WO', I, J)
  IF (J .NE. 0) STOP 'SY'
  WRITE (UNIT=I, IOSTAT=J) SV
  IF (J .NE. 0) STOP 'S'
  Y = QZERO
  IF (Z .EQ. QZERO) THEN
     ! read G again
     CALL BFOPEN(TRIM(CLA)//'.YX', 'RO', I, J)
     IF (J .NE. 0) CALL BFOPEN(TRIM(CLA)//'.Y', 'RO', I, J)
     IF (J .NE. 0) STOP 'Y'
     READ (UNIT=I, IOSTAT=J) G
     IF (J .NE. 0) STOP 'G'
     CLOSE(I)
     ALLOCATE(W(M,N))
     DO I = 1, M
        DO J = 1, N
           W(I,J) = G(I,J)
           Z = HYPOT(Z, W(I,J))
           DO L = 1, N
              ! W(I,J) = W(I,J) - U(I,L) * REAL(V(L,J), REAL128)
              W(I,J) = IEEE_FMA(U(I,L), REAL(-V(L,J), REAL128), W(I,J))
           END DO
           Y = HYPOT(Y, W(I,J))
        END DO
     END DO
     Y = Y / Z
     IF (ALLOCATED(W)) DEALLOCATE(W)
     IF (ALLOCATED(U)) DEALLOCATE(U)
  END IF
  WRITE (OUTPUT_UNIT,'(ES30.21E4)') REAL(Y, c_long_double)
  IF (ALLOCATED(SV)) DEALLOCATE(SV)
  IF (ALLOCATED(WRK)) DEALLOCATE(WRK)
  IF (ALLOCATED(V)) DEALLOCATE(V)
  IF (ALLOCATED(G)) DEALLOCATE(G)
#else
  STOP 'xjsvdt.exe must be compiled with GNU Fortran on x64'
#endif
END PROGRAM XJSVDT

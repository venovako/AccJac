PROGRAM ZJSVDT
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, OUTPUT_UNIT, REAL64, REAL128
  IMPLICIT NONE
  REAL(KIND=REAL128), PARAMETER :: QZERO = 0.0_REAL128, QONE = 1.0_REAL128
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=REAL128) :: X, Y, Z
  COMPLEX(KIND=REAL128) :: H
  COMPLEX(KIND=REAL64) :: T
  REAL(KIND=REAL64) :: R
  INTEGER(KIND=INT64) :: CLK(3)
  INTEGER :: M, N, LDG, LDV, JPOS, GS, INFO, I, J, L
  INTEGER(KIND=INT64), ALLOCATABLE :: JV(:)
  COMPLEX(KIND=REAL64), ALLOCATABLE :: G(:,:), V(:,:), WRK(:,:)
  REAL(KIND=REAL64), ALLOCATABLE :: SV(:), LY(:)
  COMPLEX(KIND=REAL128), ALLOCATABLE :: U(:,:), W(:,:)
  EXTERNAL :: ZJSVDC, ZJSVDR
  ! read the command line arguments
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .NE. 5) STOP 'zjsvdt.exe M N JPOS OPTS FILE'
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
  CALL GET_COMMAND_ARGUMENT(4, CLA)
  READ (CLA,*) L
  IF (L .LT. 0) STOP 'OPTS'
  L = IAND(L, 7)
  CALL GET_COMMAND_ARGUMENT(2, CLA)
  READ (CLA,*) N
  IF (N .LT. 0) THEN
     N = -N
     L = IOR(L, 8)
  END IF
  IF ((N .EQ. 0) .OR. (N .GT. M)) STOP 'N'
  CALL GET_COMMAND_ARGUMENT(3, CLA)
  READ (CLA,*) JPOS
  IF ((JPOS .LT. -1) .OR. (JPOS .GT. N)) STOP 'JPOS'
  CALL GET_COMMAND_ARGUMENT(5, CLA)
  IF (LEN_TRIM(CLA) .LE. 0) STOP 'FILE'
  ! check J
  IF (JPOS .EQ. -1) THEN
     ALLOCATE(JV(N))
     CALL BFOPEN(TRIM(CLA)//'.J', 'RO', I, J)
     IF (J .NE. 0) STOP 'J'
     READ (UNIT=I, IOSTAT=J) JV
     IF (J .NE. 0) STOP 'JV'
     CLOSE(I)
     JPOS = 0
     J = 1
     DO WHILE (JV(J) .EQ. 1_INT64)
        JPOS = JPOS + 1
        J = J + 1
        IF (J .GT. N) EXIT
     END DO
     IF (J .LE. N) THEN
        DO WHILE (JV(J) .EQ. -1_INT64)
           J = J + 1
           IF (J .GT. N) EXIT
        END DO
        IF (J .LE. N) STOP 'J not [+I,-I]'
     END IF
     DEALLOCATE(JV)
  END IF
  ! set G
  LDG = M
  ALLOCATE(G(LDG,N))
  CALL BFOPEN(TRIM(CLA)//'.Y', 'RO', I, J)
  IF (J .NE. 0) STOP 'Y'
  READ (UNIT=I, IOSTAT=J) G
  IF (J .NE. 0) STOP 'G'
  CLOSE(I)
  ! allocate the rest
  LDV = N
  ALLOCATE(V(LDV,N))
  INFO = IAND(L, 3)
  IF (IAND(L, 8) .EQ. 0) THEN
     ALLOCATE(WRK(M,N))
  ELSE ! track off
     ALLOCATE(WRK(2*M,N))
     INFO = IOR(INFO, 4)
  END IF
  ALLOCATE(SV(N))
  ! call ZJSVDC
  GS = HUGE(GS)
  CALL SYSTEM_CLOCK(CLK(1), CLK(2), CLK(3))
  IF (IAND(L, 4) .EQ. 0) THEN
     CALL ZJSVDC(M, N, G, LDG, V, LDV, JPOS, SV, WRK, GS, INFO)
  ELSE ! ZJSVDR
     CALL ZJSVDR(M, N, G, LDG, V, LDV, JPOS, SV, WRK, GS, INFO)
  END IF
  CALL SYSTEM_CLOCK(CLK(3))
  CLK(1) = CLK(3) - CLK(1)
  CLK(3) = MOD(CLK(1), CLK(2)) * 1000000_INT64
  CLK(1) = CLK(1) / CLK(2)
  CLK(3) = CLK(3) / CLK(2)
  R = REAL(WRK(1,1))
  CLK(2) = TRANSFER(R, 0_INT64)
  WRITE (OUTPUT_UNIT,'(I11,A,I12,A,I6,A,I8,A,I6.6,A)',ADVANCE='NO') INFO, ',', CLK(2), ',', GS, ',', CLK(1), '.', CLK(3), ','
  FLUSH(OUTPUT_UNIT)
  IF (INFO .LT. 0) THEN
     IF (IAND(L, 4) .EQ. 0) THEN
        STOP 'ZJSVDC'
     ELSE ! ZJSVDR
        STOP 'ZJSVDR'
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
           U(I,J) = CMPLX(&
                REAL(REAL(G(I,J)), REAL128) * Y,&
                REAL(AIMAG(G(I,J)), REAL128) * Y, REAL128)
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
  ! read LY
  ALLOCATE(LY(N))
  CALL BFOPEN(TRIM(CLA)//'.LY', 'RO', I, J)
  IF (J .NE. 0) STOP 'LY'
  READ (UNIT=I, IOSTAT=J) LY
  IF (J .NE. 0) STOP 'L'
  CLOSE(I)
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
  X = QZERO
  DO J = 1, JPOS
     Y = SV(J)
     Y = ABS(IEEE_FMA(Y, Y, REAL(-LY(J), REAL128)) / LY(J))
     X = MAX(X, Y)
  END DO
  DO J = JPOS+1, N
     Y = SV(J)
     Y = ABS(IEEE_FMA(Y, Y, REAL(LY(J), REAL128)) / LY(J))
     X = MAX(X, Y)
  END DO
  DEALLOCATE(LY)
  WRITE (OUTPUT_UNIT,'(ES25.17E3,A)',ADVANCE='NO') REAL(X, REAL64), ','
  FLUSH(OUTPUT_UNIT)
  Y = QZERO
  IF (Z .EQ. QZERO) THEN
     ! read G again
     CALL BFOPEN(TRIM(CLA)//'.Y', 'RO', I, J)
     IF (J .NE. 0) STOP 'Y'
     READ (UNIT=I, IOSTAT=J) G
     IF (J .NE. 0) STOP 'G'
     CLOSE(I)
     ALLOCATE(W(M,N))
     DO I = 1, M
        DO J = 1, N
           W(I,J) = G(I,J)
           Z = HYPOT(Z, HYPOT(REAL(W(I,J)), AIMAG(W(I,J))))
           DO L = 1, N
              ! W(I,J) = W(I,J) - U(I,L) * V(L,J)
              H = CMPLX(REAL(-REAL(V(L,J)), REAL128), REAL(-AIMAG(V(L,J)), REAL128), REAL128)
              W(I,J) = CMPLX(&
                   IEEE_FMA(REAL(U(I,L)), REAL(H), IEEE_FMA(-AIMAG(U(I,L)), AIMAG(H), REAL(W(I,J)))),&
                   IEEE_FMA(REAL(U(I,L)), AIMAG(H), IEEE_FMA(AIMAG(U(I,L)), REAL(H), AIMAG(W(I,J)))), REAL128)
           END DO
           Y = HYPOT(Y, HYPOT(REAL(W(I,J)), AIMAG(W(I,J))))
        END DO
     END DO
     Y = Y / Z
     IF (ALLOCATED(W)) DEALLOCATE(W)
     IF (ALLOCATED(U)) DEALLOCATE(U)
  END IF
  WRITE (OUTPUT_UNIT,'(ES25.17E3)') REAL(Y, REAL64)
  IF (ALLOCATED(SV)) DEALLOCATE(SV)
  IF (ALLOCATED(WRK)) DEALLOCATE(WRK)
  IF (ALLOCATED(V)) DEALLOCATE(V)
  IF (ALLOCATED(G)) DEALLOCATE(G)
CONTAINS
#include "bfopen.f90"
END PROGRAM ZJSVDT

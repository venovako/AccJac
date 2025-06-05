PROGRAM DJSVDX
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long, c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, OUTPUT_UNIT, REAL64
#else
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, OUTPUT_UNIT, REAL64, REAL128
#endif
  IMPLICIT NONE
  INTERFACE
     PURE FUNCTION XFMA(A, B, C)
#ifdef __GFORTRAN__
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
       IMPLICIT NONE
#ifdef __GFORTRAN__
       REAL(KIND=c_long_double), INTENT(IN) :: A, B, C
       REAL(KIND=c_long_double) :: XFMA
#else
       REAL(KIND=REAL128), INTENT(IN) :: A, B, C
       REAL(KIND=REAL128) :: XFMA
#endif
     END FUNCTION XFMA
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
  REAL(KIND=KK), PARAMETER :: QZERO = 0.0_KK, QONE = 1.0_KK
  CHARACTER(LEN=256) :: CLA
  REAL(KIND=KK) :: X, Y, Z
  REAL(KIND=REAL64) :: T
  INTEGER(KIND=INT64) :: CLK(3)
  INTEGER :: M, N, LDG, LDV, JPOS, GS, INFO, I, J, L
  INTEGER(KIND=INT64), ALLOCATABLE :: JV(:)
  REAL(KIND=REAL64), ALLOCATABLE :: G(:,:), V(:,:), WRK(:,:), SV(:), LY(:)
  INTEGER, ALLOCATABLE :: IX(:)
  REAL(KIND=KK), ALLOCATABLE :: U(:,:), W(:,:)
  EXTERNAL :: BFOPEN, DJSVDF
  ! read the command line arguments
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .NE. 5) STOP 'djsvdx.exe M N JPOS OPTS FILE'
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
  IF ((JPOS .LT. -1) .OR. (JPOS .GT. N)) STOP 'JPOS'
  CALL GET_COMMAND_ARGUMENT(4, CLA)
  READ (CLA,*) L
  IF (L .LT. 0) STOP 'OPTS'
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
  ALLOCATE(WRK(M,N))
  ALLOCATE(SV(N))
  ALLOCATE(IX(N))
  ! call DJSVDF
  GS = HUGE(GS)
  INFO = L
  CALL SYSTEM_CLOCK(CLK(1), CLK(2), CLK(3))
  CALL DJSVDF(M, N, G, LDG, V, LDV, JPOS, SV, GS, IX, WRK, INFO)
  CALL SYSTEM_CLOCK(CLK(3))
  CLK(1) = CLK(3) - CLK(1)
  CLK(3) = MOD(CLK(1), CLK(2)) * 1000000_INT64
  CLK(1) = CLK(1) / CLK(2)
  CLK(3) = CLK(3) / CLK(2)
  T = WRK(1,1)
  CLK(2) = INT(T, INT64)
  WRITE (OUTPUT_UNIT,'(I11,A,I12,A,I6,A,I8,A,I6.6,A)',ADVANCE='NO') INFO, ',', CLK(2), ',', GS, ',', CLK(1), '.', CLK(3), ','
  FLUSH(OUTPUT_UNIT)
  IF (INFO .LT. 0) STOP 'DJSVDF'
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
           U(I,J) = REAL(G(I,J), KK) * Y
        END DO
     END DO
  END IF
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
  IF (J .NE. 0) STOP 'SY'
  WRITE (UNIT=I, IOSTAT=J) SV
  IF (J .NE. 0) STOP 'S'
  ! read LY
  CALL BFOPEN(TRIM(CLA)//'.LY', 'RO', I, J)
  IF (J .EQ. 0) THEN
     ALLOCATE(LY(N))
     READ (UNIT=I, IOSTAT=J) LY
     IF (J .NE. 0) STOP 'L'
     CLOSE(I)
     L = 1
     I = 1
     DO WHILE (L .GT. 0)
        L = 0
        DO J = 1, N-I
           IF (LY(J) .LT. LY(J+1)) THEN
              T = LY(J)
              LY(J) = LY(J+1)
              LY(J+1) = T
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
              T = SV(J)
              SV(J) = SV(J+1)
              SV(J+1) = T
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
              T = SV(J)
              SV(J) = SV(J+1)
              SV(J+1) = T
              L = L + 1
           END IF
        END DO
        I = I + 1
     END DO
     X = QZERO
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
     DEALLOCATE(LY)
  ELSE ! no LY
     X = -QZERO
  END IF
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
           Z = HYPOTX(Z, W(I,J))
           DO L = 1, N
              ! W(I,J) = W(I,J) - U(I,L) * REAL(V(L,J), KK)
              W(I,J) = XFMA(U(I,L), REAL(-V(L,J), KK), W(I,J))
           END DO
           Y = HYPOTX(Y, W(I,J))
        END DO
     END DO
     Y = Y / Z
     IF (ALLOCATED(W)) DEALLOCATE(W)
     IF (ALLOCATED(U)) DEALLOCATE(U)
  END IF
  WRITE (OUTPUT_UNIT,'(ES25.17E3)') REAL(Y, REAL64)
  IF (ALLOCATED(IX)) DEALLOCATE(IX)
  IF (ALLOCATED(SV)) DEALLOCATE(SV)
  IF (ALLOCATED(WRK)) DEALLOCATE(WRK)
  IF (ALLOCATED(V)) DEALLOCATE(V)
  IF (ALLOCATED(G)) DEALLOCATE(G)
END PROGRAM DJSVDX

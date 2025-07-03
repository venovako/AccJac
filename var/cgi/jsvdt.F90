PROGRAM JSVDT
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE
  INTERFACE
     FUNCTION CGICMAIN(ARGC, ARGV) BIND(C,NAME='cgicMain')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       INTEGER(KIND=c_int), INTENT(IN), VALUE :: ARGC
       TYPE(c_ptr), INTENT(IN) :: ARGV(ARGC)
       INTEGER(KIND=c_int) :: CGICMAIN
     END FUNCTION CGICMAIN
  END INTERFACE
  INTERFACE
     FUNCTION CGIMAIN(U, V) BIND(C,NAME='cgiMain')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       INTEGER(KIND=c_int), INTENT(IN), VALUE :: U, V
       INTEGER(KIND=c_int) :: CGIMAIN
     END FUNCTION CGIMAIN
  END INTERFACE
  INTEGER, PARAMETER :: ARGLEN = 131072
  CHARACTER(LEN=ARGLEN,KIND=c_char), ALLOCATABLE, TARGET :: ARGV0
  TYPE(c_ptr) :: ARGV(1)
  INTEGER :: RET, U, M, N, J
  CHARACTER :: T
  EXTERNAL :: BFOPEN
  U = COMMAND_ARGUMENT_COUNT()
  IF (U .EQ. 0) THEN
     OPEN(NEWUNIT=U, STATUS='SCRATCH', ACTION='WRITE', ACCESS='SEQUENTIAL', FORM='FORMATTED')
     ALLOCATE(ARGV0)
     CALL GET_COMMAND_ARGUMENT(0, ARGV0)
     ARGV0 = TRIM(ARGV0)//c_null_char
     ARGV(1) = C_LOC(ARGV0)
     RET = INT(CGICMAIN(1_c_int, ARGV))
     DEALLOCATE(ARGV0)
     IF (RET .EQ. 0) RET = INT(CGIMAIN(INT(U,c_int), INT(FNUM(U),c_int)))
     CLOSE(U)
  ELSE IF (U .GT. 0) THEN
     ALLOCATE(ARGV0)
     CALL GET_COMMAND_ARGUMENT(0, ARGV0)
     IF (U .NE. 6) STOP TRIM(ARGV0)//' T M N J O F'
     CALL GET_COMMAND_ARGUMENT(1, T)
     CALL GET_COMMAND_ARGUMENT(2, ARGV0)
     READ (ARGV0,*) M
     IF (M .LE. 0) STOP 'M'
     CALL GET_COMMAND_ARGUMENT(3, ARGV0)
     READ (ARGV0,*) N
     IF ((N .LE. 0) .OR. (N .GT. M)) STOP 'N'
     CALL GET_COMMAND_ARGUMENT(4, ARGV0)
     READ (ARGV0,*) J
     IF ((J .LT. 0) .OR. (J .GT. N)) STOP 'J'
     CALL GET_COMMAND_ARGUMENT(5, ARGV0)
     READ (ARGV0,*) U
     IF ((U .LT. 0) .OR. (U .GT. 7)) STOP 'O'
     CALL GET_COMMAND_ARGUMENT(6, ARGV0)
     IF (LEN_TRIM(ARGV0) .LE. 0) STOP 'F'
     SELECT CASE (T)
     CASE ('C','c')
        CALL CEXE(M, N, J, U, ARGV0)
     CASE ('D','d')
        CALL DEXE(M, N, J, U, ARGV0)
     CASE ('S','s')
        CALL SEXE(M, N, J, U, ARGV0)
     CASE ('W','w')
        CALL WEXE(M, N, J, U, ARGV0)
     CASE ('X','x')
        CALL XEXE(M, N, J, U, ARGV0)
     CASE ('Z','z')
        CALL ZEXE(M, N, J, U, ARGV0)
     CASE DEFAULT
        STOP 'T'
     END SELECT
     DEALLOCATE(ARGV0)
     RET = 0
  ELSE ! U < 0
     ! should never happen
     RET = 1
  END IF
CONTAINS
  SUBROUTINE CEXE(M, N, J, O, F)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, REAL32
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL32
    INTEGER, INTENT(IN) :: M, N, J, O
    CHARACTER(LEN=*), INTENT(IN) :: F
    COMPLEX(KIND=K), ALLOCATABLE :: G(:,:), V(:,:), WRK(:,:)
    REAL(KIND=K), ALLOCATABLE :: SV(:), RWRK(:)
    INTEGER, ALLOCATABLE :: IX(:)
    INTEGER :: GS, I, L
    EXTERNAL :: BFOPEN, CJSVDF
    ALLOCATE(G(M,N))
    ALLOCATE(V(N,N))
    ALLOCATE(WRK(M,N))
    ALLOCATE(SV(N))
    ALLOCATE(RWRK(N))
    ALLOCATE(IX(N))
    CALL BFOPEN(TRIM(F)//'.Y', 'RO', I, L)
    IF (L .NE. 0) STOP 'Y'
    READ (UNIT=I, IOSTAT=L) G
    IF (L .NE. 0) STOP 'G'
    GS = HUGE(GS)
    IX(1) = ERROR_UNIT
    CALL CJSVDF(M, N, G, M, V, N, J, SV, GS, IX, WRK, RWRK, L)
    WRITE (OUTPUT_UNIT,'(3(A,I11))') 'CJSVDF:', L, ',', INT(RWRK(N)), ',', GS
    FLUSH(OUTPUT_UNIT)
    CALL BFOPEN(TRIM(F)//'.YU', 'WO', I, L)
    IF (L .NE. 0) STOP 'YU'
    WRITE (UNIT=I, IOSTAT=L) G
    IF (L .NE. 0) STOP 'U'
    CLOSE(I)
    CALL BFOPEN(TRIM(F)//'.YV', 'WO', I, L)
    IF (L .NE. 0) STOP 'YV'
    WRITE (UNIT=I, IOSTAT=L) V
    IF (L .NE. 0) STOP 'V'
    CLOSE(I)
    CALL BFOPEN(TRIM(F)//'.SS', 'WO', I, L)
    IF (L .NE. 0) STOP 'SS'
    WRITE (UNIT=I, IOSTAT=L) SV
    IF (L .NE. 0) STOP 's'
    CLOSE(I)
    ! V^-1 = J V^H J
    DO L = 1, N
       DO I = 1, L-1
          WRK(1,1) = V(I,L)
          V(I,L) = CONJG(V(L,I))
          V(L,I) = CONJG(WRK(1,1))
       END DO
       V(L,L) = CONJG(V(L,L))
    END DO
    DO L = 1, J
       DO I = J+1, N
          V(I,L) = -V(I,L)
       END DO
    END DO
    DO L = J+1, N
       DO I = 1, J
          V(I,L) = -V(I,L)
       END DO
    END DO
    CALL BFOPEN(TRIM(F)//'.ZZ', 'WO', I, L)
    IF (L .NE. 0) STOP 'ZZ'
    WRITE (UNIT=I, IOSTAT=L) V
    IF (L .NE. 0) STOP 'Z'
    CLOSE(I)
    IF (GS .NE. 0) THEN
       L = -GS
       DO I = 1, N
          SV(I) = SCALE(SV(I), L)
       END DO
    END IF
    CALL BFOPEN(TRIM(F)//'.SV', 'WO', I, L)
    IF (L .NE. 0) STOP 'SV'
    WRITE (UNIT=I, IOSTAT=L) SV
    IF (L .NE. 0) STOP 'S'
    CLOSE(I)
    DEALLOCATE(IX)
    DEALLOCATE(RWRK)
    DEALLOCATE(SV)
    DEALLOCATE(WRK)
    DEALLOCATE(V)
    DEALLOCATE(G)
  END SUBROUTINE CEXE
  SUBROUTINE DEXE(M, N, J, O, F)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, REAL64
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL64
    INTEGER, INTENT(IN) :: M, N, J, O
    CHARACTER(LEN=*), INTENT(IN) :: F
    REAL(KIND=K), ALLOCATABLE :: G(:,:), V(:,:), WRK(:,:), SV(:), RWRK(:)
    INTEGER, ALLOCATABLE :: IX(:)
    INTEGER :: GS, I, L
    EXTERNAL :: BFOPEN, DJSVDF
    ALLOCATE(G(M,N))
    ALLOCATE(V(N,N))
    ALLOCATE(WRK(M,N))
    ALLOCATE(SV(N))
    ALLOCATE(RWRK(N))
    ALLOCATE(IX(N))
    CALL BFOPEN(TRIM(F)//'.Y', 'RO', I, L)
    IF (L .NE. 0) STOP 'Y'
    READ (UNIT=I, IOSTAT=L) G
    IF (L .NE. 0) STOP 'G'
    GS = HUGE(GS)
    IX(1) = ERROR_UNIT
    CALL DJSVDF(M, N, G, M, V, N, J, SV, GS, IX, WRK, RWRK, L)
    WRITE (OUTPUT_UNIT,'(3(A,I11))') 'DJSVDF:', L, ',', INT(RWRK(N)), ',', GS
    FLUSH(OUTPUT_UNIT)
    CALL BFOPEN(TRIM(F)//'.YU', 'WO', I, L)
    IF (L .NE. 0) STOP 'YU'
    WRITE (UNIT=I, IOSTAT=L) G
    IF (L .NE. 0) STOP 'U'
    CLOSE(I)
    CALL BFOPEN(TRIM(F)//'.YV', 'WO', I, L)
    IF (L .NE. 0) STOP 'YV'
    WRITE (UNIT=I, IOSTAT=L) V
    IF (L .NE. 0) STOP 'V'
    CLOSE(I)
    CALL BFOPEN(TRIM(F)//'.SS', 'WO', I, L)
    IF (L .NE. 0) STOP 'SS'
    WRITE (UNIT=I, IOSTAT=L) SV
    IF (L .NE. 0) STOP 's'
    CLOSE(I)
    ! V^-1 = J V^T J
    DO L = 2, N
       DO I = 1, L-1
          WRK(1,1) = V(I,L)
          V(I,L) = V(L,I)
          V(L,I) = WRK(1,1)
       END DO
    END DO
    DO L = 1, J
       DO I = J+1, N
          V(I,L) = -V(I,L)
       END DO
    END DO
    DO L = J+1, N
       DO I = 1, J
          V(I,L) = -V(I,L)
       END DO
    END DO
    CALL BFOPEN(TRIM(F)//'.ZZ', 'WO', I, L)
    IF (L .NE. 0) STOP 'ZZ'
    WRITE (UNIT=I, IOSTAT=L) V
    IF (L .NE. 0) STOP 'Z'
    CLOSE(I)
    IF (GS .NE. 0) THEN
       L = -GS
       DO I = 1, N
          SV(I) = SCALE(SV(I), L)
       END DO
    END IF
    CALL BFOPEN(TRIM(F)//'.SV', 'WO', I, L)
    IF (L .NE. 0) STOP 'SV'
    WRITE (UNIT=I, IOSTAT=L) SV
    IF (L .NE. 0) STOP 'S'
    CLOSE(I)
    DEALLOCATE(IX)
    DEALLOCATE(RWRK)
    DEALLOCATE(SV)
    DEALLOCATE(WRK)
    DEALLOCATE(V)
    DEALLOCATE(G)
  END SUBROUTINE DEXE
  SUBROUTINE SEXE(M, N, J, O, F)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, REAL32
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL32
    INTEGER, INTENT(IN) :: M, N, J, O
    CHARACTER(LEN=*), INTENT(IN) :: F
    REAL(KIND=K), ALLOCATABLE :: G(:,:), V(:,:), WRK(:,:), SV(:), RWRK(:)
    INTEGER, ALLOCATABLE :: IX(:)
    INTEGER :: GS, I, L
    EXTERNAL :: BFOPEN, SJSVDF
    ALLOCATE(G(M,N))
    ALLOCATE(V(N,N))
    ALLOCATE(WRK(M,N))
    ALLOCATE(SV(N))
    ALLOCATE(RWRK(N))
    ALLOCATE(IX(N))
    CALL BFOPEN(TRIM(F)//'.Y', 'RO', I, L)
    IF (L .NE. 0) STOP 'Y'
    READ (UNIT=I, IOSTAT=L) G
    IF (L .NE. 0) STOP 'G'
    GS = HUGE(GS)
    IX(1) = ERROR_UNIT
    CALL SJSVDF(M, N, G, M, V, N, J, SV, GS, IX, WRK, RWRK, L)
    WRITE (OUTPUT_UNIT,'(3(A,I11))') 'SJSVDF:', L, ',', INT(RWRK(N)), ',', GS
    FLUSH(OUTPUT_UNIT)
    CALL BFOPEN(TRIM(F)//'.YU', 'WO', I, L)
    IF (L .NE. 0) STOP 'YU'
    WRITE (UNIT=I, IOSTAT=L) G
    IF (L .NE. 0) STOP 'U'
    CLOSE(I)
    CALL BFOPEN(TRIM(F)//'.YV', 'WO', I, L)
    IF (L .NE. 0) STOP 'YV'
    WRITE (UNIT=I, IOSTAT=L) V
    IF (L .NE. 0) STOP 'V'
    CLOSE(I)
    CALL BFOPEN(TRIM(F)//'.SS', 'WO', I, L)
    IF (L .NE. 0) STOP 'SS'
    WRITE (UNIT=I, IOSTAT=L) SV
    IF (L .NE. 0) STOP 's'
    CLOSE(I)
    ! V^-1 = J V^T J
    DO L = 2, N
       DO I = 1, L-1
          WRK(1,1) = V(I,L)
          V(I,L) = V(L,I)
          V(L,I) = WRK(1,1)
       END DO
    END DO
    DO L = 1, J
       DO I = J+1, N
          V(I,L) = -V(I,L)
       END DO
    END DO
    DO L = J+1, N
       DO I = 1, J
          V(I,L) = -V(I,L)
       END DO
    END DO
    CALL BFOPEN(TRIM(F)//'.ZZ', 'WO', I, L)
    IF (L .NE. 0) STOP 'ZZ'
    WRITE (UNIT=I, IOSTAT=L) V
    IF (L .NE. 0) STOP 'Z'
    CLOSE(I)
    IF (GS .NE. 0) THEN
       L = -GS
       DO I = 1, N
          SV(I) = SCALE(SV(I), L)
       END DO
    END IF
    CALL BFOPEN(TRIM(F)//'.SV', 'WO', I, L)
    IF (L .NE. 0) STOP 'SV'
    WRITE (UNIT=I, IOSTAT=L) SV
    IF (L .NE. 0) STOP 'S'
    CLOSE(I)
    DEALLOCATE(IX)
    DEALLOCATE(RWRK)
    DEALLOCATE(SV)
    DEALLOCATE(WRK)
    DEALLOCATE(V)
    DEALLOCATE(G)
  END SUBROUTINE SEXE
  SUBROUTINE WEXE(M, N, J, O, F)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = c_long_double
    INTEGER, INTENT(IN) :: M, N, J, O
    CHARACTER(LEN=*), INTENT(IN) :: F
    COMPLEX(KIND=K), ALLOCATABLE :: G(:,:), V(:,:), WRK(:,:)
    REAL(KIND=K), ALLOCATABLE :: SV(:), RWRK(:)
    INTEGER, ALLOCATABLE :: IX(:)
    INTEGER :: GS, I, L
    EXTERNAL :: BFOPEN, WBRD1, WBWR1, XBWR1, WJSVDF
    ALLOCATE(G(M,N))
    ALLOCATE(V(N,N))
    ALLOCATE(WRK(M,N))
    ALLOCATE(SV(N))
    ALLOCATE(RWRK(N))
    ALLOCATE(IX(N))
    CALL BFOPEN(TRIM(F)//'.Y', 'RO', I, L)
    IF (L .NE. 0) STOP 'Y'
    CALL WBRD1(I, M * N, G, L)
    IF (L .NE. 0) STOP 'G'
    GS = HUGE(GS)
    IX(1) = ERROR_UNIT
    CALL WJSVDF(M, N, G, M, V, N, J, SV, GS, IX, WRK, RWRK, L)
    WRITE (OUTPUT_UNIT,'(3(A,I11))') 'WJSVDF:', L, ',', INT(RWRK(N)), ',', GS
    FLUSH(OUTPUT_UNIT)
    CALL BFOPEN(TRIM(F)//'.YU', 'WO', I, L)
    IF (L .NE. 0) STOP 'YU'
    CALL WBWR1(I, M * N, G, L)
    IF (L .NE. 0) STOP 'U'
    CLOSE(I)
    CALL BFOPEN(TRIM(F)//'.YV', 'WO', I, L)
    IF (L .NE. 0) STOP 'YV'
    CALL WBWR1(I, N * N, V, L)
    IF (L .NE. 0) STOP 'V'
    CLOSE(I)
    CALL BFOPEN(TRIM(F)//'.SS', 'WO', I, L)
    IF (L .NE. 0) STOP 'SS'
    CALL XBWR1(I, N, SV, L)
    IF (L .NE. 0) STOP 's'
    CLOSE(I)
    ! V^-1 = J V^H J
    DO L = 1, N
       DO I = 1, L-1
          WRK(1,1) = V(I,L)
          V(I,L) = CONJG(V(L,I))
          V(L,I) = CONJG(WRK(1,1))
       END DO
       V(L,L) = CONJG(V(L,L))
    END DO
    DO L = 1, J
       DO I = J+1, N
          V(I,L) = -V(I,L)
       END DO
    END DO
    DO L = J+1, N
       DO I = 1, J
          V(I,L) = -V(I,L)
       END DO
    END DO
    CALL BFOPEN(TRIM(F)//'.ZZ', 'WO', I, L)
    IF (L .NE. 0) STOP 'ZZ'
    CALL WBWR1(I, N * N, V, L)
    IF (L .NE. 0) STOP 'Z'
    CLOSE(I)
    IF (GS .NE. 0) THEN
       L = -GS
       DO I = 1, N
          SV(I) = SCALE(SV(I), L)
       END DO
    END IF
    CALL BFOPEN(TRIM(F)//'.SV', 'WO', I, L)
    IF (L .NE. 0) STOP 'SV'
    CALL XBWR1(I, N, SV, L)
    IF (L .NE. 0) STOP 'S'
    CLOSE(I)
    DEALLOCATE(IX)
    DEALLOCATE(RWRK)
    DEALLOCATE(SV)
    DEALLOCATE(WRK)
    DEALLOCATE(V)
    DEALLOCATE(G)
  END SUBROUTINE WEXE
  SUBROUTINE XEXE(M, N, J, O, F)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = c_long_double
    INTEGER, INTENT(IN) :: M, N, J, O
    CHARACTER(LEN=*), INTENT(IN) :: F
    REAL(KIND=K), ALLOCATABLE :: G(:,:), V(:,:), WRK(:,:), SV(:), RWRK(:)
    INTEGER, ALLOCATABLE :: IX(:)
    INTEGER :: GS, I, L
    EXTERNAL :: BFOPEN, XBRD1, XBWR1, XJSVDF
    ALLOCATE(G(M,N))
    ALLOCATE(V(N,N))
    ALLOCATE(WRK(M,N))
    ALLOCATE(SV(N))
    ALLOCATE(RWRK(N))
    ALLOCATE(IX(N))
    CALL BFOPEN(TRIM(F)//'.Y', 'RO', I, L)
    IF (L .NE. 0) STOP 'Y'
    CALL XBRD1(I, M * N, G, L)
    IF (L .NE. 0) STOP 'G'
    GS = HUGE(GS)
    IX(1) = ERROR_UNIT
    CALL XJSVDF(M, N, G, M, V, N, J, SV, GS, IX, WRK, RWRK, L)
    WRITE (OUTPUT_UNIT,'(3(A,I11))') 'XJSVDF:', L, ',', INT(RWRK(N)), ',', GS
    FLUSH(OUTPUT_UNIT)
    CALL BFOPEN(TRIM(F)//'.YU', 'WO', I, L)
    IF (L .NE. 0) STOP 'YU'
    CALL XBWR1(I, M * N, G, L)
    IF (L .NE. 0) STOP 'U'
    CLOSE(I)
    CALL BFOPEN(TRIM(F)//'.YV', 'WO', I, L)
    IF (L .NE. 0) STOP 'YV'
    CALL XBWR1(I, N * N, V, L)
    IF (L .NE. 0) STOP 'V'
    CLOSE(I)
    CALL BFOPEN(TRIM(F)//'.SS', 'WO', I, L)
    IF (L .NE. 0) STOP 'SS'
    CALL XBWR1(I, N, SV, L)
    IF (L .NE. 0) STOP 's'
    CLOSE(I)
    ! V^-1 = J V^T J
    DO L = 2, N
       DO I = 1, L-1
          WRK(1,1) = V(I,L)
          V(I,L) = V(L,I)
          V(L,I) = WRK(1,1)
       END DO
    END DO
    DO L = 1, J
       DO I = J+1, N
          V(I,L) = -V(I,L)
       END DO
    END DO
    DO L = J+1, N
       DO I = 1, J
          V(I,L) = -V(I,L)
       END DO
    END DO
    CALL BFOPEN(TRIM(F)//'.ZZ', 'WO', I, L)
    IF (L .NE. 0) STOP 'ZZ'
    CALL XBWR1(I, N * N, V, L)
    IF (L .NE. 0) STOP 'Z'
    CLOSE(I)
    IF (GS .NE. 0) THEN
       L = -GS
       DO I = 1, N
          SV(I) = SCALE(SV(I), L)
       END DO
    END IF
    CALL BFOPEN(TRIM(F)//'.SV', 'WO', I, L)
    IF (L .NE. 0) STOP 'SV'
    CALL XBWR1(I, N, SV, L)
    IF (L .NE. 0) STOP 'S'
    CLOSE(I)
    CLOSE(I)
    DEALLOCATE(IX)
    DEALLOCATE(RWRK)
    DEALLOCATE(SV)
    DEALLOCATE(WRK)
    DEALLOCATE(V)
    DEALLOCATE(G)
  END SUBROUTINE XEXE
  SUBROUTINE ZEXE(M, N, J, O, F)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT, REAL64
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL64
    INTEGER, INTENT(IN) :: M, N, J, O
    CHARACTER(LEN=*), INTENT(IN) :: F
    COMPLEX(KIND=K), ALLOCATABLE :: G(:,:), V(:,:), WRK(:,:)
    REAL(KIND=K), ALLOCATABLE :: SV(:), RWRK(:)
    INTEGER, ALLOCATABLE :: IX(:)
    INTEGER :: GS, I, L
    EXTERNAL :: BFOPEN, ZJSVDF
    ALLOCATE(G(M,N))
    ALLOCATE(V(N,N))
    ALLOCATE(WRK(M,N))
    ALLOCATE(SV(N))
    ALLOCATE(RWRK(N))
    ALLOCATE(IX(N))
    CALL BFOPEN(TRIM(F)//'.Y', 'RO', I, L)
    IF (L .NE. 0) STOP 'Y'
    READ (UNIT=I, IOSTAT=L) G
    IF (L .NE. 0) STOP 'G'
    GS = HUGE(GS)
    IX(1) = ERROR_UNIT
    CALL ZJSVDF(M, N, G, M, V, N, J, SV, GS, IX, WRK, RWRK, L)
    WRITE (OUTPUT_UNIT,'(3(A,I11))') 'ZJSVDF:', L, ',', INT(RWRK(N)), ',', GS
    FLUSH(OUTPUT_UNIT)
    CALL BFOPEN(TRIM(F)//'.YU', 'WO', I, L)
    IF (L .NE. 0) STOP 'YU'
    WRITE (UNIT=I, IOSTAT=L) G
    IF (L .NE. 0) STOP 'U'
    CLOSE(I)
    CALL BFOPEN(TRIM(F)//'.YV', 'WO', I, L)
    IF (L .NE. 0) STOP 'YV'
    WRITE (UNIT=I, IOSTAT=L) V
    IF (L .NE. 0) STOP 'V'
    CLOSE(I)
    CALL BFOPEN(TRIM(F)//'.SS', 'WO', I, L)
    IF (L .NE. 0) STOP 'SS'
    WRITE (UNIT=I, IOSTAT=L) SV
    IF (L .NE. 0) STOP 's'
    CLOSE(I)
    ! V^-1 = J V^H J
    DO L = 1, N
       DO I = 1, L-1
          WRK(1,1) = V(I,L)
          V(I,L) = CONJG(V(L,I))
          V(L,I) = CONJG(WRK(1,1))
       END DO
       V(L,L) = CONJG(V(L,L))
    END DO
    DO L = 1, J
       DO I = J+1, N
          V(I,L) = -V(I,L)
       END DO
    END DO
    DO L = J+1, N
       DO I = 1, J
          V(I,L) = -V(I,L)
       END DO
    END DO
    CALL BFOPEN(TRIM(F)//'.ZZ', 'WO', I, L)
    IF (L .NE. 0) STOP 'ZZ'
    WRITE (UNIT=I, IOSTAT=L) V
    IF (L .NE. 0) STOP 'Z'
    CLOSE(I)
    IF (GS .NE. 0) THEN
       L = -GS
       DO I = 1, N
          SV(I) = SCALE(SV(I), L)
       END DO
    END IF
    CALL BFOPEN(TRIM(F)//'.SV', 'WO', I, L)
    IF (L .NE. 0) STOP 'SV'
    WRITE (UNIT=I, IOSTAT=L) SV
    IF (L .NE. 0) STOP 'S'
    CLOSE(I)
    DEALLOCATE(IX)
    DEALLOCATE(RWRK)
    DEALLOCATE(SV)
    DEALLOCATE(WRK)
    DEALLOCATE(V)
    DEALLOCATE(G)
  END SUBROUTINE ZEXE
END PROGRAM JSVDT

SUBROUTINE WTRCOA(N, A, LDA, AS, S, T, U)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#endif
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
!   INTERFACE
!      PURE FUNCTION WNRMOA(N, A, LDA, AS)
! #ifdef __GFORTRAN__
!        USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
! #endif
!        USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
!        IMPLICIT NONE
!        INTEGER, INTENT(IN) :: N, LDA, AS
! #ifdef __GFORTRAN__
!        COMPLEX(KIND=c_long_double), INTENT(IN) :: A(LDA,N)
! #else
!        COMPLEX(KIND=REAL128), INTENT(IN) :: A(LDA,N)
! #endif
!        REAL(KIND=REAL128) :: WNRMOA
!      END FUNCTION WNRMOA
!   END INTERFACE
#ifdef __GFORTRAN__
  INTEGER, PARAMETER :: K = c_long_double
#else
  INTEGER, PARAMETER :: K = REAL128
#endif
  INTEGER, INTENT(IN) :: N, LDA, AS, S
  COMPLEX(KIND=K), INTENT(IN) :: A(LDA,N)
  INTEGER, INTENT(INOUT) :: T, U
  INTEGER :: NS, MD, H1
  CHARACTER(LEN=8) :: FN
  REAL(KIND=REAL128), EXTERNAL :: WNRMOA
  IF ((LDA .LT. N) .OR. (N .LE. 1) .OR. (N .GE. 1000)) RETURN
  IF (S .EQ. 0) THEN
     WRITE (FN,'(A,I3.3,A)') CHAR(T), N, '.csv'
     OPEN(NEWUNIT=U, IOSTAT=T, FILE=FN, STATUS='REPLACE', ACTION='WRITE', ACCESS='SEQUENTIAL', FORM='FORMATTED')
     IF (T .EQ. 0) THEN
        WRITE (U,'(A)') '"TRANSF", "SWEEP", "off(A)"'
        FLUSH(U)
     ELSE ! ERROR
        RETURN
     END IF
  END IF
  NS = (N * (N - 1)) / 2
  MD = MOD(T, NS)
  H1 = N / 2
  IF ((MD .EQ. 0) .OR. (MD .EQ. 1) .OR. (MOD(MD, H1) .EQ. 0)) THEN
#ifdef __GFORTRAN__
     WRITE (U,'(I11,A,I3,A,ES30.21E4)') T, ',', S, ',', WNRMOA(N, A, LDA, AS)
#else
     WRITE (U,'(I11,A,I3,A,ES45.36E4)') T, ',', S, ',', WNRMOA(N, A, LDA, AS)
#endif
     FLUSH(U)
  END IF
  IF (S .LT. 0) THEN
     CLOSE(U)
  ELSE ! S >= 0
     T = T + 1
  END IF
END SUBROUTINE WTRCOA

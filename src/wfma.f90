PURE FUNCTION WFMA(A, B, C, D)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
#ifdef __GFORTRAN__
#ifdef MPC
  INTERFACE
     PURE SUBROUTINE MPC_WFMA(E, A, B, C, D)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       COMPLEX(KIND=c_long_double), INTENT(OUT) :: E
       COMPLEX(KIND=c_long_double), INTENT(IN) :: A, B, C
       REAL(KIND=c_long_double), INTENT(IN) :: D
     END SUBROUTINE MPC_WFMA
  END INTERFACE
#endif
  COMPLEX(KIND=c_long_double), INTENT(IN) :: A, B, C
  REAL(KIND=c_long_double), INTENT(IN) :: D
  COMPLEX(KIND=c_long_double) :: WFMA
#else
  COMPLEX(KIND=REAL128), INTENT(IN) :: A, B, C
  REAL(KIND=REAL128), INTENT(IN) :: D
  COMPLEX(KIND=REAL128) :: WFMA
#endif
#ifdef MPC
#ifdef __GFORTRAN__
  CALL MPC_WFMA(WFMA, A, B, C, D)
#else
  WFMA = (A * B + C) * D
#endif
#else
  WFMA = (A * B + C) * D
#endif
END FUNCTION WFMA

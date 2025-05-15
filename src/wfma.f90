PURE FUNCTION WFMA(A, B, C)
#ifdef __GFORTRAN__
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
  IMPLICIT NONE
#ifdef __GFORTRAN__
#ifdef MPC
  INTERFACE
     PURE SUBROUTINE MPC_WFMA(D, A, B, C)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       COMPLEX(KIND=c_long_double), INTENT(OUT) :: D
       COMPLEX(KIND=c_long_double), INTENT(IN) :: A, B, C
     END SUBROUTINE MPC_WFMA
  END INTERFACE
#endif
  COMPLEX(KIND=c_long_double), INTENT(IN) :: A, B, C
  COMPLEX(KIND=c_long_double) :: WFMA
#else
  COMPLEX(KIND=REAL128), INTENT(IN) :: A, B, C
  COMPLEX(KIND=REAL128) :: WFMA
#endif
#ifdef MPC
#ifdef __GFORTRAN__
  COMPLEX(KIND=c_long_double) :: D
  CALL MPC_WFMA(D, A, B, C)
  WFMA = D
#else
  WFMA = A * B + C
#endif
#else
  WFMA = A * B + C
#endif
END FUNCTION WFMA

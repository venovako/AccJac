#!/bin/bash
for I in ABS CBRT NEG REC_SQRT SQR SQRT
do
    echo "  PURE SUBROUTINE MPFR_${I}_F(ROP, OP, RND, I)"
    echo "    IMPLICIT NONE"
    echo "    INTERFACE"
    echo "       PURE FUNCTION MPFR_${I}(ROP, OP, RND) BIND(C)"
    echo "         IMPORT"
    echo "         IMPLICIT NONE"
    echo "         TYPE(c_ptr), INTENT(IN), VALUE :: ROP, OP"
    echo "         INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), VALUE :: RND"
    echo "         INTEGER(KIND=c_int) :: MPFR_${I}"
    echo "       END FUNCTION MPFR_${I}"
    echo "    END INTERFACE"
    echo "    TYPE(MPFR_T), INTENT(INOUT), TARGET :: ROP"
    echo "    TYPE(MPFR_T), INTENT(IN), TARGET :: OP"
    echo "    INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), OPTIONAL :: RND"
    echo "    INTEGER, INTENT(OUT), OPTIONAL :: I"
    echo "    INTEGER(KIND=c_int) :: INFO"
    echo "    INFO = 0_c_int"
    echo "    IF (OP%TAG .EQ. TAG_NULL) INFO = -3_c_int"
    echo "    IF (ROP%TAG .EQ. TAG_NULL) INFO = -2_c_int"
    echo "    IF (INFO .EQ. 0_c_int) THEN"
    echo "       IF (PRESENT(RND)) THEN"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), C_LOC(OP), RND)"
    echo "       ELSE ! default rounding"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), C_LOC(OP), MPFR_RND_DEFAULT)"
    echo "       END IF"
    echo "    END IF"
    echo "    IF (PRESENT(I)) I = INT(INFO)"
    echo "  END SUBROUTINE MPFR_${I}_F"
done
for I in ADD DIV HYPOT MUL SUB
do
    echo "  PURE SUBROUTINE MPFR_${I}_F(ROP, OP1, OP2, RND, I)"
    echo "    IMPLICIT NONE"
    echo "    INTERFACE"
    echo "       PURE FUNCTION MPFR_${I}(ROP, OP1, OP2, RND) BIND(C)"
    echo "         IMPORT"
    echo "         IMPLICIT NONE"
    echo "         TYPE(c_ptr), INTENT(IN), VALUE :: ROP, OP1, OP2"
    echo "         INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), VALUE :: RND"
    echo "         INTEGER(KIND=c_int) :: MPFR_${I}"
    echo "       END FUNCTION MPFR_${I}"
    echo "    END INTERFACE"
    echo "    TYPE(MPFR_T), INTENT(INOUT), TARGET :: ROP"
    echo "    TYPE(MPFR_T), INTENT(IN), TARGET :: OP1, OP2"
    echo "    INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), OPTIONAL :: RND"
    echo "    INTEGER, INTENT(OUT), OPTIONAL :: I"
    echo "    INTEGER(KIND=c_int) :: INFO"
    echo "    INFO = 0_c_int"
    echo "    IF (OP2%TAG .EQ. TAG_NULL) INFO = -4_c_int"
    echo "    IF (OP1%TAG .EQ. TAG_NULL) INFO = -3_c_int"
    echo "    IF (ROP%TAG .EQ. TAG_NULL) INFO = -2_c_int"
    echo "    IF (INFO .EQ. 0_c_int) THEN"
    echo "       IF (PRESENT(RND)) THEN"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), C_LOC(OP1), C_LOC(OP2), RND)"
    echo "       ELSE ! default rounding"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), C_LOC(OP1), C_LOC(OP2), MPFR_RND_DEFAULT)"
    echo "       END IF"
    echo "    END IF"
    echo "    IF (PRESENT(I)) I = INT(INFO)"
    echo "  END SUBROUTINE MPFR_${I}_F"
done
for I in ADD_SI DIV_SI MUL_SI SUB_SI
do
    echo "  PURE SUBROUTINE MPFR_${I}_F(ROP, OP1, OP2, RND, I)"
    echo "    IMPLICIT NONE"
    echo "    INTERFACE"
    echo "       PURE FUNCTION MPFR_${I}(ROP, OP1, OP2, RND) BIND(C)"
    echo "         IMPORT"
    echo "         IMPLICIT NONE"
    echo "         TYPE(c_ptr), INTENT(IN), VALUE :: ROP, OP1"
    echo "         INTEGER(KIND=c_long), INTENT(IN), VALUE :: OP2"
    echo "         INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), VALUE :: RND"
    echo "         INTEGER(KIND=c_int) :: MPFR_${I}"
    echo "       END FUNCTION MPFR_${I}"
    echo "    END INTERFACE"
    echo "    TYPE(MPFR_T), INTENT(INOUT), TARGET :: ROP"
    echo "    TYPE(MPFR_T), INTENT(IN), TARGET :: OP1"
    echo "    INTEGER(KIND=c_long), INTENT(IN) :: OP2"
    echo "    INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), OPTIONAL :: RND"
    echo "    INTEGER, INTENT(OUT), OPTIONAL :: I"
    echo "    INTEGER(KIND=c_int) :: INFO"
    echo "    INFO = 0_c_int"
    echo "    IF (OP1%TAG .EQ. TAG_NULL) INFO = -3_c_int"
    echo "    IF (ROP%TAG .EQ. TAG_NULL) INFO = -2_c_int"
    echo "    IF (INFO .EQ. 0_c_int) THEN"
    echo "       IF (PRESENT(RND)) THEN"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), C_LOC(OP1), OP2, RND)"
    echo "       ELSE ! default rounding"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), C_LOC(OP1), OP2, MPFR_RND_DEFAULT)"
    echo "       END IF"
    echo "    END IF"
    echo "    IF (PRESENT(I)) I = INT(INFO)"
    echo "  END SUBROUTINE MPFR_${I}_F"
done
for I in SI_DIV SI_SUB
do
    echo "  PURE SUBROUTINE MPFR_${I}_F(ROP, OP1, OP2, RND, I)"
    echo "    IMPLICIT NONE"
    echo "    INTERFACE"
    echo "       PURE FUNCTION MPFR_${I}(ROP, OP1, OP2, RND) BIND(C)"
    echo "         IMPORT"
    echo "         IMPLICIT NONE"
    echo "         TYPE(c_ptr), INTENT(IN), VALUE :: ROP, OP2"
    echo "         INTEGER(KIND=c_long), INTENT(IN), VALUE :: OP1"
    echo "         INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), VALUE :: RND"
    echo "         INTEGER(KIND=c_int) :: MPFR_${I}"
    echo "       END FUNCTION MPFR_${I}"
    echo "    END INTERFACE"
    echo "    TYPE(MPFR_T), INTENT(INOUT), TARGET :: ROP"
    echo "    INTEGER(KIND=c_long), INTENT(IN) :: OP1"
    echo "    TYPE(MPFR_T), INTENT(IN), TARGET :: OP2"
    echo "    INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), OPTIONAL :: RND"
    echo "    INTEGER, INTENT(OUT), OPTIONAL :: I"
    echo "    INTEGER(KIND=c_int) :: INFO"
    echo "    INFO = 0_c_int"
    echo "    IF (OP2%TAG .EQ. TAG_NULL) INFO = -4_c_int"
    echo "    IF (ROP%TAG .EQ. TAG_NULL) INFO = -2_c_int"
    echo "    IF (INFO .EQ. 0_c_int) THEN"
    echo "       IF (PRESENT(RND)) THEN"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), OP1, C_LOC(OP2), RND)"
    echo "       ELSE ! default rounding"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), OP1, C_LOC(OP2), MPFR_RND_DEFAULT)"
    echo "       END IF"
    echo "    END IF"
    echo "    IF (PRESENT(I)) I = INT(INFO)"
    echo "  END SUBROUTINE MPFR_${I}_F"
done
for I in ADD_D DIV_D MUL_D SUB_D
do
    echo "  PURE SUBROUTINE MPFR_${I}_F(ROP, OP1, OP2, RND, I)"
    echo "    IMPLICIT NONE"
    echo "    INTERFACE"
    echo "       PURE FUNCTION MPFR_${I}(ROP, OP1, OP2, RND) BIND(C)"
    echo "         IMPORT"
    echo "         IMPLICIT NONE"
    echo "         TYPE(c_ptr), INTENT(IN), VALUE :: ROP, OP1"
    echo "         REAL(KIND=c_double), INTENT(IN), VALUE :: OP2"
    echo "         INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), VALUE :: RND"
    echo "         INTEGER(KIND=c_int) :: MPFR_${I}"
    echo "       END FUNCTION MPFR_${I}"
    echo "    END INTERFACE"
    echo "    TYPE(MPFR_T), INTENT(INOUT), TARGET :: ROP"
    echo "    TYPE(MPFR_T), INTENT(IN), TARGET :: OP1"
    echo "    REAL(KIND=REAL64), INTENT(IN) :: OP2"
    echo "    INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), OPTIONAL :: RND"
    echo "    INTEGER, INTENT(OUT), OPTIONAL :: I"
    echo "    INTEGER(KIND=c_int) :: INFO"
    echo "    INFO = 0_c_int"
    echo "    IF (OP1%TAG .EQ. TAG_NULL) INFO = -3_c_int"
    echo "    IF (ROP%TAG .EQ. TAG_NULL) INFO = -2_c_int"
    echo "    IF (INFO .EQ. 0_c_int) THEN"
    echo "       IF (PRESENT(RND)) THEN"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), C_LOC(OP1), OP2, RND)"
    echo "       ELSE ! default rounding"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), C_LOC(OP1), OP2, MPFR_RND_DEFAULT)"
    echo "       END IF"
    echo "    END IF"
    echo "    IF (PRESENT(I)) I = INT(INFO)"
    echo "  END SUBROUTINE MPFR_${I}_F"
done
for I in D_DIV D_SUB
do
    echo "  PURE SUBROUTINE MPFR_${I}_F(ROP, OP1, OP2, RND, I)"
    echo "    IMPLICIT NONE"
    echo "    INTERFACE"
    echo "       PURE FUNCTION MPFR_${I}(ROP, OP1, OP2, RND) BIND(C)"
    echo "         IMPORT"
    echo "         IMPLICIT NONE"
    echo "         TYPE(c_ptr), INTENT(IN), VALUE :: ROP, OP2"
    echo "         REAL(KIND=c_double), INTENT(IN), VALUE :: OP1"
    echo "         INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), VALUE :: RND"
    echo "         INTEGER(KIND=c_int) :: MPFR_${I}"
    echo "       END FUNCTION MPFR_${I}"
    echo "    END INTERFACE"
    echo "    TYPE(MPFR_T), INTENT(INOUT), TARGET :: ROP"
    echo "    REAL(KIND=REAL64), INTENT(IN) :: OP1"
    echo "    TYPE(MPFR_T), INTENT(IN), TARGET :: OP2"
    echo "    INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), OPTIONAL :: RND"
    echo "    INTEGER, INTENT(OUT), OPTIONAL :: I"
    echo "    INTEGER(KIND=c_int) :: INFO"
    echo "    INFO = 0_c_int"
    echo "    IF (OP2%TAG .EQ. TAG_NULL) INFO = -4_c_int"
    echo "    IF (ROP%TAG .EQ. TAG_NULL) INFO = -2_c_int"
    echo "    IF (INFO .EQ. 0_c_int) THEN"
    echo "       IF (PRESENT(RND)) THEN"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), OP1, C_LOC(OP2), RND)"
    echo "       ELSE ! default rounding"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), OP1, C_LOC(OP2), MPFR_RND_DEFAULT)"
    echo "       END IF"
    echo "    END IF"
    echo "    IF (PRESENT(I)) I = INT(INFO)"
    echo "  END SUBROUTINE MPFR_${I}_F"
done
for I in FMA FMS
do
    echo "  PURE SUBROUTINE MPFR_${I}_F(ROP, OP1, OP2, OP3, RND, I)"
    echo "    IMPLICIT NONE"
    echo "    INTERFACE"
    echo "       PURE FUNCTION MPFR_${I}(ROP, OP1, OP2, OP3, RND) BIND(C)"
    echo "         IMPORT"
    echo "         IMPLICIT NONE"
    echo "         TYPE(c_ptr), INTENT(IN), VALUE :: ROP, OP1, OP2, OP3"
    echo "         INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), VALUE :: RND"
    echo "         INTEGER(KIND=c_int) :: MPFR_${I}"
    echo "       END FUNCTION MPFR_${I}"
    echo "    END INTERFACE"
    echo "    TYPE(MPFR_T), INTENT(INOUT), TARGET :: ROP"
    echo "    TYPE(MPFR_T), INTENT(IN), TARGET :: OP1, OP2, OP3"
    echo "    INTEGER(KIND=MPFR_RND_KIND), INTENT(IN), OPTIONAL :: RND"
    echo "    INTEGER, INTENT(OUT), OPTIONAL :: I"
    echo "    INTEGER(KIND=c_int) :: INFO"
    echo "    INFO = 0_c_int"
    echo "    IF (OP3%TAG .EQ. TAG_NULL) INFO = -5_c_int"
    echo "    IF (OP2%TAG .EQ. TAG_NULL) INFO = -4_c_int"
    echo "    IF (OP1%TAG .EQ. TAG_NULL) INFO = -3_c_int"
    echo "    IF (ROP%TAG .EQ. TAG_NULL) INFO = -2_c_int"
    echo "    IF (INFO .EQ. 0_c_int) THEN"
    echo "       IF (PRESENT(RND)) THEN"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), C_LOC(OP1), C_LOC(OP2), C_LOC(OP3), RND)"
    echo "       ELSE ! default rounding"
    echo "          INFO = MPFR_${I}(C_LOC(ROP), C_LOC(OP1), C_LOC(OP2), C_LOC(OP3), MPFR_RND_DEFAULT)"
    echo "       END IF"
    echo "    END IF"
    echo "    IF (PRESENT(I)) I = INT(INFO)"
    echo "  END SUBROUTINE MPFR_${I}_F"
done
unset I

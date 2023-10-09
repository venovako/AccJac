SUBROUTINE ZJAEVD(JOB, N, A, LDA, U, LDU, S, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
  IMPLICIT NONE

  INTERFACE
     FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       REAL(c_double), INTENT(IN), VALUE :: X, Y
       REAL(c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
  INTERFACE
     SUBROUTINE DJAEV2(A, B, C, RT1, RT2, CS1, SN1)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       REAL(c_double), INTENT(IN), TARGET :: A, B, C
       REAL(c_double), INTENT(OUT), TARGET :: RT1, RT2, CS1, SN1
     END SUBROUTINE DJAEV2
  END INTERFACE
  INTERFACE
     SUBROUTINE DLAEV2(A, B, C, RT1, RT2, CS1, SN1)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       REAL(c_double), INTENT(IN), TARGET :: A, B, C
       REAL(c_double), INTENT(OUT), TARGET :: RT1, RT2, CS1, SN1
     END SUBROUTINE DLAEV2
  END INTERFACE
  INTERFACE
     SUBROUTINE ZJAEV2(A, B, C, RT1, RT2, CS1, SN1)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       COMPLEX(c_double), INTENT(IN) :: A, B, C
       REAL(c_double), INTENT(OUT), TARGET :: RT1, RT2, CS1
       COMPLEX(c_double), INTENT(OUT) :: SN1
     END SUBROUTINE ZJAEV2
  END INTERFACE
  INTERFACE
     SUBROUTINE ZLAEV2(A, B, C, RT1, RT2, CS1, SN1)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       COMPLEX(c_double), INTENT(IN) :: A, B, C
       REAL(c_double), INTENT(OUT), TARGET :: RT1, RT2, CS1
       COMPLEX(c_double), INTENT(OUT) :: SN1
     END SUBROUTINE ZLAEV2
  END INTERFACE

  REAL(c_double), PARAMETER :: ZERO = 0.0_c_double, ONE = 1.0_c_double
  COMPLEX(c_double), PARAMETER :: CZERO = CMPLX(ZERO, ZERO, c_double), CONE = CMPLX(ONE, ZERO, c_double)
  INTEGER, PARAMETER :: ESHFT = 4, MININT = -HUGE(0) - 1

  INTEGER, INTENT(IN) :: JOB, N, LDA, LDU
  COMPLEX(c_double), INTENT(INOUT) :: A(LDA,N), U(LDU,N)
  INTEGER, INTENT(INOUT) :: S, INFO

  COMPLEX(c_double) :: SN1, AP, AQ
  REAL(c_double), TARGET :: AR, AI, AA, XA, RT1, RT2, CS1
  LOGICAL :: UPPER, ACCVEC, LAPACK, IDENT
  INTEGER :: MAXSTP, I, J, K, L, M, P, Q, DBGU

  IF (INFO .GE. 0) THEN
     IDENT = .TRUE.
     MAXSTP = INFO
  ELSE ! INFO .LT. 0
     IDENT = .FALSE.
     MAXSTP = -(INFO + 1)
  END IF
  IF (S .GT. 0) THEN
     DBGU = -S
  ELSE IF (S .LT. 0) THEN
     DBGU = -(S + 1)
  ELSE ! S = 0
     DBGU = MININT
  END IF

  IF (LDU .LT. N) THEN
     INFO = -6
  ELSE IF (LDA .LT. N) THEN
     INFO = -4
  ELSE IF (N .LT. 0) THEN
     INFO = -2
  ELSE IF ((JOB .LT. 0) .OR. (JOB .GT. 7)) THEN
     INFO = -1
  ELSE ! dimensions OK
     INFO = 0
  END IF
  IF (INFO .NE. 0) RETURN
  UPPER = (IAND(JOB, 1) .NE. 0)
  ACCVEC = (IAND(JOB, 2) .NE. 0)
  LAPACK = (IAND(JOB, 4) .NE. 0)
  IF (N .EQ. 0) RETURN

  IF (N .GE. 2) THEN
     L = MININT
     M = L
     IF (UPPER) THEN
        DO J = 1, N
           DO I = 1, J-1
              AR = ABS(REAL(A(I,J)))
              AI = ABS(AIMAG(A(I,J)))
              IF ((.NOT. (AR .LE. HUGE(ZERO))) .OR. (.NOT. (AI .LE. HUGE(ZERO)))) THEN
                 IF (DBGU .NE. MININT) WRITE (DBGU,*) I, ',', J, ',', A(I,J)
                 INFO = -3
                 RETURN
              END IF
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
           AR = ABS(REAL(A(J,J)))
           AI = AIMAG(A(J,J))
           IF ((.NOT. (AR .LE. HUGE(ZERO))) .OR. (.NOT. (AI .EQ. ZERO))) THEN
              IF (DBGU .NE. MININT) WRITE (DBGU,*) J, ',', A(J,J)
              INFO = -3
              RETURN
           END IF
           IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
        END DO
     ELSE ! LOWER
        DO J = 1, N
           AR = ABS(REAL(A(J,J)))
           AI = AIMAG(A(J,J))
           IF ((.NOT. (AR .LE. HUGE(ZERO))) .OR. (.NOT. (AI .EQ. ZERO))) THEN
              IF (DBGU .NE. MININT) WRITE (DBGU,*) J, ',', A(J,J)
              INFO = -3
              RETURN
           END IF
           IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
           DO I = J+1, N
              AR = ABS(REAL(A(I,J)))
              AI = ABS(AIMAG(A(I,J)))
              IF ((.NOT. (AR .LE. HUGE(ZERO))) .OR. (.NOT. (AI .LE. HUGE(ZERO)))) THEN
                 IF (DBGU .NE. MININT) WRITE (DBGU,*) I, ',', J, ',', A(I,J)
                 INFO = -3
                 RETURN
              END IF
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
        END DO
     END IF
  ELSE ! N = 1
     IF (AIMAG(A(1,1)) .NE. ZERO) THEN
        IF (DBGU .NE. MININT) WRITE (DBGU,*) A(J,J)
        INFO = -3
     ELSE ! A OK
        A(1,1) = CMPLX(REAL(A(1,1)), ZERO, c_double)
        IF (ACCVEC) THEN
           IF (IDENT) THEN
              U(1,1) = CONE
           ELSE ! check U
              AR = ABS(REAL(U(1,1)))
              AI = ABS(AIMAG(U(1,1)))
              IF ((.NOT. (AR .LE. HUGE(ZERO))) .OR. (.NOT. (AI .LE. HUGE(ZERO)))) INFO = -5
           END IF
        END IF
     END IF
     IF (INFO .EQ. 0) S = 0
     RETURN
  END IF

  IF (ACCVEC) THEN
     IF (IDENT) THEN
        DO J = 1, N
           DO I = 1, J-1
              U(I,J) = CZERO
           END DO
           U(J,J) = CONE
           DO I = J+1, N
              U(I,J) = CZERO
           END DO
        END DO
     ELSE ! check U
        DO J = 1, N
           DO I = 1, N
              AR = ABS(REAL(U(I,J)))
              AI = ABS(AIMAG(U(I,J)))
              IF ((.NOT. (AR .LE. HUGE(ZERO))) .OR. (.NOT. (AI .LE. HUGE(ZERO)))) THEN
                 INFO = -5
                 RETURN
              END IF
           END DO
        END DO
     END IF
  END IF

  IF (L .EQ. M) THEN
     IF (DBGU .NE. MININT) WRITE (DBGU,'(A)') '[INFO] A is a nul-matrix'
     S = 0
     RETURN
  END IF
  S = EXPONENT(HUGE(ZERO)) - L - ESHFT
  IF (DBGU .NE. MININT) WRITE (DBGU,'(A,I5)') '[INFO] Scaling A by 2**', S
  L = 0
  M = 0
  IF (S .EQ. 0) THEN
     IF (UPPER) THEN
        DO J = 1, N
           DO I = 1, J-1
              AR = REAL(A(I,J))
              AI = AIMAG(A(I,J))
              IF (AR .NE. ZERO) L = L + 1
              IF (AI .NE. ZERO) M = M + 1
           END DO
           AR = REAL(A(J,J))
           A(J,J) = CMPLX(AR, ZERO, c_double)
        END DO
     ELSE ! LOWER
        DO J = 1, N
           AR = REAL(A(J,J))
           A(J,J) = CMPLX(AR, ZERO, c_double)
           DO I = J+1, N
              AR = REAL(A(I,J))
              AI = AIMAG(A(I,J))
              IF (AR .NE. ZERO) L = L + 1
              IF (AI .NE. ZERO) M = M + 1
           END DO
        END DO
     END IF
  ELSE ! S .NE. 0
     IF (UPPER) THEN
        DO J = 1, N
           DO I = 1, J-1
              AR = SCALE(REAL(A(I,J)), S)
              AI = SCALE(AIMAG(A(I,J)), S)
              IF (AR .NE. ZERO) L = L + 1
              IF (AI .NE. ZERO) M = M + 1
              A(I,J) = CMPLX(AR, AI, c_double)
           END DO
           AR = SCALE(REAL(A(J,J)), S)
           A(J,J) = CMPLX(AR, ZERO, c_double)
        END DO
     ELSE ! LOWER
        DO J = 1, N
           AR = SCALE(REAL(A(J,J)), S)
           A(J,J) = CMPLX(AR, ZERO, c_double)
           DO I = J+1, N
              AR = SCALE(REAL(A(I,J)), S)
              AI = SCALE(AIMAG(A(I,J)), S)
              IF (AR .NE. ZERO) L = L + 1
              IF (AI .NE. ZERO) M = M + 1
              A(I,J) = CMPLX(AR, AI, c_double)
           END DO
        END DO
     END IF
  END IF
  IF (M .EQ. 0) THEN
     IF (L .EQ. 0) THEN
        ! diagonal A, only the sorting required
        IF (DBGU .NE. MININT) WRITE (DBGU,'(A)') '[INFO] A is diagonal'
     ELSE ! real A
        IF (DBGU .NE. MININT) WRITE (DBGU,'(A)') '[INFO] A is real'
     END IF
  ELSE IF (L .EQ. 0) THEN
     ! imaginary off-A
     IF (DBGU .NE. MININT) WRITE (DBGU,'(A)') '[INFO] off-diagonal of A is imaginary'
  END IF

  K = 0
  DO WHILE (.TRUE.)
     IF (MAXSTP .GT. 0) THEN
        IF (K .LT. MAXSTP) THEN
           K = K + 1
        ELSE ! K .GE. MAXSTP
           EXIT
        END IF
     ELSE IF (MAXSTP .EQ. 0) THEN
        K = K + 1
     ELSE ! MAXSTP .LT. 0
        INFO = -8
        RETURN
     END IF
     XA = -ONE
     IF (UPPER) THEN
        DO J = 2, N
           DO I = 1, J-1
              AR = ABS(REAL(A(I,J)))
              AI = ABS(AIMAG(A(I,J)))
              AA = CR_HYPOT(AR, AI)
              AR = REAL(A(I,I))
              AI = REAL(A(J,J))
              IF ((AA .GT. XA) .OR. ((AA .EQ. XA) .AND. &
                   ((AR .LT. AI) .OR. ((AR .EQ. ZERO) .AND. (AI .EQ. ZERO) .AND. (SIGN(ONE, AR) .LT. SIGN(ONE, AI)))))) THEN
                 XA = AA
                 P = I
                 Q = J
              END IF
           END DO
        END DO
     ELSE ! LOWER
        DO J = 1, N-1
           DO I = J+1, N
              AR = ABS(REAL(A(I,J)))
              AI = ABS(AIMAG(A(I,J)))
              AA = CR_HYPOT(AR, AI)
              AR = REAL(A(J,J))
              AI = REAL(A(I,I))
              IF ((AA .GT. XA) .OR. ((AA .EQ. XA) .AND. &
                   ((AR .LT. AI) .OR. ((AR .EQ. ZERO) .AND. (AI .EQ. ZERO) .AND. (SIGN(ONE, AR) .LT. SIGN(ONE, AI)))))) THEN
                 XA = AA
                 P = J
                 Q = I
              END IF
           END DO
        END DO
     END IF
     IF (XA .EQ. ZERO) THEN
        AR = REAL(A(P,P))
        AI = REAL(A(Q,Q))
        IF ((AR .LT. AI) .OR. ((AR .EQ. ZERO) .AND. (AI .EQ. ZERO) .AND. (SIGN(ONE, AR) .LT. SIGN(ONE, AI)))) THEN
           IF (DBGU .NE. MININT) WRITE (DBGU,'(2(A,I2))') '[INFO] Swapping ', P, ' and ', Q
           A(P,P) = CMPLX(AI, ZERO, c_double)
           A(Q,Q) = CMPLX(AR, ZERO, c_double)
           IF (UPPER) THEN
              DO I = 1, P-1
                 SN1 = A(I,P)
                 A(I,P) = A(I,Q)
                 A(I,Q) = SN1
              END DO
              DO I = P+1, Q-1
                 SN1 = CONJG(A(I,Q))
                 A(I,Q) = CONJG(A(P,I))
                 A(P,I) = SN1
              END DO
              DO J = Q+1, N
                 SN1 = A(P,J)
                 A(P,J) = A(Q,J)
                 A(Q,J) = SN1
              END DO
           ELSE ! LOWER
              DO I = Q+1, N
                 SN1 = A(I,P)
                 A(I,P) = A(I,Q)
                 A(I,Q) = SN1
              END DO
              DO I = P+1, Q-1
                 SN1 = CONJG(A(I,P))
                 A(I,P) = CONJG(A(Q,I))
                 A(Q,I) = SN1
              END DO
              DO J = 1, P-1
                 SN1 = A(P,J)
                 A(P,J) = A(Q,J)
                 A(Q,J) = SN1
              END DO
           END IF
           IF (ACCVEC) THEN
              DO I = 1, N
                 SN1 = U(I,P)
                 U(I,P) = U(I,Q)
                 U(I,Q) = SN1
              END DO
           END IF
           IF (UPPER) THEN
              A(P,Q) = CZERO
           ELSE ! LOWER
              A(Q,P) = CZERO
           END IF
           CYCLE
        ELSE ! the eigenvalues are in the non-ascending order
           EXIT
        END IF
     ELSE IF (UPPER) THEN
        IF (AIMAG(A(P,Q)) .EQ. ZERO) THEN
           AR = REAL(A(P,P))
           AI = REAL(A(P,Q))
           AA = REAL(A(Q,Q))
           AP = CMPLX(XA, ZERO, c_double)
           IF (LAPACK) THEN
              CALL DLAEV2(AR, AI, AA, RT1, RT2, CS1, XA)
           ELSE ! DJAEV2
              CALL DJAEV2(AR, AI, AA, RT1, RT2, CS1, XA)
           END IF
           SN1 = CMPLX(XA, ZERO, c_double)
           XA = REAL(AP)
        ELSE ! complex
           AP = CMPLX(XA, ONE, c_double)
           IF (LAPACK) THEN
              CALL ZLAEV2(A(P,P), A(P,Q), A(Q,Q), RT1, RT2, CS1, SN1)
           ELSE ! CJAEV2
              CALL ZJAEV2(A(P,P), A(P,Q), A(Q,Q), RT1, RT2, CS1, SN1)
           END IF
        END IF
     ELSE ! LOWER
        IF (AIMAG(A(Q,P)) .EQ. ZERO) THEN
           AR = REAL(A(P,P))
           AI = REAL(A(Q,P))
           AA = REAL(A(Q,Q))
           AP = CMPLX(XA, ZERO, c_double)
           IF (LAPACK) THEN
              CALL DLAEV2(AR, AI, AA, RT1, RT2, CS1, XA)
           ELSE ! DJAEV2
              CALL DJAEV2(AR, AI, AA, RT1, RT2, CS1, XA)
           END IF
           SN1 = CMPLX(XA, ZERO, c_double)
           XA = REAL(AP)
        ELSE ! complex
           AP = CMPLX(XA, ONE, c_double)
           IF (LAPACK) THEN
              CALL ZLAEV2(A(P,P), CONJG(A(Q,P)), A(Q,Q), RT1, RT2, CS1, SN1)
           ELSE ! ZJAEV2
              CALL ZJAEV2(A(P,P), CONJG(A(Q,P)), A(Q,Q), RT1, RT2, CS1, SN1)
           END IF
        END IF
     END IF
     IF (UPPER) THEN
        A(P,Q) = CZERO
     ELSE ! LOWER
        A(Q,P) = CZERO
     END IF
     IDENT = ((RT1 .GE. RT2) .OR. ((RT1 .EQ. ZERO) .AND. (RT2 .EQ. ZERO) .AND. (SIGN(ONE, RT1) .LT. SIGN(ONE, RT2))))
     ! IDENT = .TRUE.
     ! U = [ CS1 -CONJG(SN1) ]
     !     [ SN1     CS1     ]
     ! IDENT = .FALSE.
     ! U = [ -CONJG(SN1) CS1 ]
     !     [     CS1     SN1 ]
     IF (DBGU .NE. MININT) WRITE (DBGU,'(I6,2(A,I2),A,ES25.17E3,2(A,L1),5(A,ES25.17E3),A)') &
          K, ',', P, ',', Q, ',', XA, ',', IDENT, ',', (AIMAG(AP) .EQ. ZERO), ',', &
          RT1, ',', RT2, ',', CS1, ',(', REAL(SN1), ',', AIMAG(SN1), ')'
     IF (IDENT) THEN
        A(P,P) = CMPLX(RT1, ZERO, c_double)
        A(Q,Q) = CMPLX(RT2, ZERO, c_double)
     ELSE ! PERM
        A(P,P) = CMPLX(RT2, ZERO, c_double)
        A(Q,Q) = CMPLX(RT1, ZERO, c_double)
     END IF
     L = MININT
     M = L
     IF (RT1 .NE. ZERO) L = MAX(L, EXPONENT(RT1))
     IF (RT2 .NE. ZERO) L = MAX(L, EXPONENT(RT2))
     IF (UPPER) THEN
        IF (IDENT) THEN
           DO I = 1, P-1
              AP = A(I,P) * CS1 + A(I,Q) * SN1
              AQ = A(I,Q) * CS1 - A(I,P) * CONJG(SN1)
              A(I,P) = AP
              AR = REAL(AP)
              AI = AIMAG(AP)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
              A(I,Q) = AQ
              AR = REAL(AQ)
              AI = AIMAG(AQ)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
           DO I = P+1, Q-1
              AP = CONJG(A(P,I)) * CS1 + A(I,Q) * SN1
              AQ = A(I,Q) * CS1 - CONJG(A(P,I)) * CONJG(SN1)
              A(P,I) = CONJG(AP)
              AR = REAL(AP)
              AI = AIMAG(AP)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
              A(I,Q) = AQ
              AR = REAL(AQ)
              AI = AIMAG(AQ)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
           DO J = Q+1, N
              AP = CS1 * A(P,J) + CONJG(SN1) * A(Q,J)
              AQ = CS1 * A(Q,J) - SN1 * A(P,J)
              A(P,J) = AP
              AR = REAL(AP)
              AI = AIMAG(AP)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
              A(Q,J) = AQ
              AR = REAL(AQ)
              AI = AIMAG(AQ)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
        ELSE ! PERM
           DO I = 1, P-1
              AP = A(I,P) * CS1 + A(I,Q) * SN1
              AQ = A(I,Q) * CS1 - A(I,P) * CONJG(SN1)
              A(I,P) = AQ
              AR = REAL(AQ)
              AI = AIMAG(AQ)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
              A(I,Q) = AP
              AR = REAL(AP)
              AI = AIMAG(AP)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
           DO I = P+1, Q-1
              AP = CONJG(A(P,I)) * CS1 + A(I,Q) * SN1
              AQ = A(I,Q) * CS1 - CONJG(A(P,I)) * CONJG(SN1)
              A(P,I) = CONJG(AQ)
              AR = REAL(AQ)
              AI = AIMAG(AQ)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
              A(I,Q) = AP
              AR = REAL(AP)
              AI = AIMAG(AP)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
           DO J = Q+1, N
              AP = CS1 * A(P,J) + CONJG(SN1) * A(Q,J)
              AQ = CS1 * A(Q,J) - SN1 * A(P,J)
              A(P,J) = AQ
              AR = REAL(AQ)
              AI = AIMAG(AQ)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
              A(Q,J) = AP
              AR = REAL(AP)
              AI = AIMAG(AP)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
        END IF
     ELSE ! LOWER
        IF (IDENT) THEN
           DO I = Q+1, N
              AP = A(I,P) * CS1 + A(I,Q) * SN1
              AQ = A(I,Q) * CS1 - A(I,P) * CONJG(SN1)
              A(I,P) = AP
              AR = REAL(AP)
              AI = AIMAG(AP)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
              A(I,Q) = AQ
              AR = REAL(AQ)
              AI = AIMAG(AQ)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
           DO I = P+1, Q-1
              AP = A(I,P) * CS1 + CONJG(A(Q,I)) * SN1
              AQ = CONJG(A(Q,I)) * CS1 - A(I,P) * CONJG(SN1)
              A(I,P) = AQ
              AR = REAL(AQ)
              AI = AIMAG(AQ)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
              A(Q,I) = CONJG(AP)
              AR = REAL(AP)
              AI = AIMAG(AP)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
           DO J = 1, P-1
              AP = CS1 * A(P,J) + CONJG(SN1) * A(Q,J)
              AQ = CS1 * A(Q,J) - SN1 * A(P,J)
              A(P,J) = AP
              AR = REAL(AP)
              AI = AIMAG(AP)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
              A(Q,J) = AQ
              AR = REAL(AQ)
              AI = AIMAG(AQ)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
        ELSE ! PERM
           DO I = Q+1, N
              AP = A(I,P) * CS1 + A(I,Q) * SN1
              AQ = A(I,Q) * CS1 - A(I,P) * CONJG(SN1)
              A(I,P) = AQ
              AR = REAL(AQ)
              AI = AIMAG(AQ)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
              A(I,Q) = AP
              AR = REAL(AP)
              AI = AIMAG(AP)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
           DO I = P+1, Q-1
              AP = A(I,P) * CS1 + CONJG(A(Q,I)) * SN1
              AQ = CONJG(A(Q,I)) * CS1 - A(I,P) * CONJG(SN1)
              A(I,P) = AP
              AR = REAL(AP)
              AI = AIMAG(AP)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
              A(Q,I) = CONJG(AQ)
              AR = REAL(AQ)
              AI = AIMAG(AQ)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
           DO J = 1, P-1
              AP = CS1 * A(P,J) + CONJG(SN1) * A(Q,J)
              AQ = CS1 * A(Q,J) - SN1 * A(P,J)
              A(P,J) = AQ
              AR = REAL(AQ)
              AI = AIMAG(AQ)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
              A(Q,J) = AP
              AR = REAL(AP)
              AI = AIMAG(AP)
              IF (AR .NE. ZERO) L = MAX(L, EXPONENT(AR))
              IF (AI .NE. ZERO) L = MAX(L, EXPONENT(AI))
           END DO
        END IF
     END IF
     IF (ACCVEC) THEN
        IF (IDENT) THEN
           DO I = 1, N
              AP = U(I,P) * CS1 + U(I,Q) * SN1
              AQ = U(I,Q) * CS1 - U(I,P) * CONJG(SN1)
              U(I,P) = AP
              U(I,Q) = AQ
           END DO
        ELSE ! PERM
           DO I = 1, N
              AP = U(I,P) * CS1 + U(I,Q) * SN1
              AQ = U(I,Q) * CS1 - U(I,P) * CONJG(SN1)
              U(I,P) = AQ
              U(I,Q) = AP
           END DO
        END IF
     END IF
     IF (L .EQ. M) THEN
        M = 0
     ELSE ! non-zero
        M = EXPONENT(HUGE(ZERO)) - L - ESHFT
     END IF
     IF (M .LT. 0) THEN
        IF (DBGU .NE. MININT) WRITE (DBGU,'(A,I4)') '[INFO] Rescaling A by 2**', M
        IF (UPPER) THEN
           DO J = 1, N
              DO I = 1, J-1
                 AR = SCALE(REAL(A(I,J)), M)
                 AI = SCALE(AIMAG(A(I,J)), M)
                 A(I,J) = CMPLX(AR, AI, c_double)
              END DO
              AR = SCALE(REAL(A(J,J)), M)
              A(J,J) = CMPLX(AR, ZERO, c_double)
           END DO
        ELSE ! LOWER
           DO J = 1, N
              AR = SCALE(REAL(A(J,J)), M)
              A(J,J) = CMPLX(AR, ZERO, c_double)
              DO I = J+1, N
                 AR = SCALE(REAL(A(I,J)), M)
                 AI = SCALE(AIMAG(A(I,J)), M)
                 A(I,J) = CMPLX(AR, AI, c_double)
              END DO
           END DO
        END IF
        S = S + M
     END IF
  END DO
  INFO = K
  S = -S
END SUBROUTINE ZJAEVD

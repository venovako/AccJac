  IF (N .LT. 0) INFO = -1
  IF (INFO .LT. 0) RETURN
  IF (IAND(INFO, 5) .EQ. 0) THEN
     IF (IAND(INFO, 8) .EQ. 0) THEN
        DO J = 1, N
           XX = CS * A(P,J) + SN * A(Q,J)
           YY = CS * A(Q,J) - SN * A(P,J)
           A(P,J) = XX
           A(Q,J) = YY
        END DO
     ELSE ! swap
        DO J = 1, N
           XX = CS * A(P,J) + SN * A(Q,J)
           YY = CS * A(Q,J) - SN * A(P,J)
           A(P,J) = YY
           A(Q,J) = XX
        END DO
     END IF
  ELSE IF (IAND(INFO, 4) .EQ. 0) THEN
     ! SN => TG
     IF (IAND(INFO, 8) .EQ. 0) THEN
        DO J = 1, N
           !DIR$ FMA
           XX = CS * (A(P,J) + SN * A(Q,J))
           !DIR$ FMA
           YY = CS * (A(Q,J) - SN * A(P,J))
           A(P,J) = XX
           A(Q,J) = YY
        END DO
     ELSE ! swap
        DO J = 1, N
           !DIR$ FMA
           XX = CS * (A(P,J) + SN * A(Q,J))
           !DIR$ FMA
           YY = CS * (A(Q,J) - SN * A(P,J))
           A(P,J) = YY
           A(Q,J) = XX
        END DO
     END IF
  ELSE IF (IAND(INFO, 8) .NE. 0) THEN
     DO J = 1, N
        XX = A(P,J)
        YY = A(Q,J)
        A(P,J) = YY
        A(Q,J) = XX
     END DO
  END IF

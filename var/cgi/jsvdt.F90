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
     FUNCTION CGIMAIN() BIND(C,NAME='cgiMain')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       INTEGER(KIND=c_int) :: CGIMAIN
     END FUNCTION CGIMAIN
  END INTERFACE
  ! TODO: this supposes that strlen(argv[0]) <= 255
  CHARACTER(LEN=256,KIND=c_char), TARGET :: ARGV0
  ! This is just a demo, so only send argv[0], since the
  ! command-line arguments are not used in cgic anyway.
  TYPE(c_ptr) :: ARGV(1)
  INTEGER :: RET
  CALL GET_COMMAND_ARGUMENT(0, ARGV0)
  ARGV0 = TRIM(ARGV0)//c_null_char
  ARGV(1) = C_LOC(ARGV0)
  RET = INT(CGICMAIN(1_c_int, ARGV))
  IF (RET .EQ. 0) RET = INT(CGIMAIN())
END PROGRAM JSVDT

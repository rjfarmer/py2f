MODULE py2f
   IMPLICIT NONE
   
   INTERFACE
      INTEGER(C_INT) FUNCTION setup() BIND(C,NAME='c_setup')
      USE ISO_C_BINDING
      END FUNCTION setup
   END INTERFACE
   
   INTERFACE
      INTEGER(C_INT) FUNCTION finish() BIND(C,NAME='c_finish')
      USE ISO_C_BINDING
      END FUNCTION finish
   END INTERFACE
   
   INTERFACE
      INTEGER(C_INT) FUNCTION run(cmd) BIND(C,NAME='c_run')
      USE ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: cmd
      END FUNCTION run
   END INTERFACE

   INTERFACE
      INTEGER(C_INT) FUNCTION setp(name,val) BIND(C,NAME='c_set_int')
      USE ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name
      INTEGER(C_INT), INTENT(IN) :: val
      END FUNCTION setp
   END INTERFACE

   INTERFACE
      INTEGER(C_INT) FUNCTION load_module(name) BIND(C,NAME='c_load_module')
      USE ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name
      END FUNCTION load_module
   END INTERFACE
   
   CONTAINS
   
   PURE FUNCTION F_C_STRING_FUNC (F_STRING) RESULT (C_STRING)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR, C_NULL_CHAR
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: F_STRING
      CHARACTER(LEN=1,KIND=C_CHAR) :: C_STRING(LEN_TRIM(F_STRING)+1)
      INTEGER                      :: N, I

      N = LEN_TRIM(F_STRING)
      DO I = 1, N
         C_STRING(I) = F_STRING(I:I)
      END DO
      C_STRING(N + 1) = C_NULL_CHAR

   END FUNCTION F_C_STRING_FUNC

   
   INTEGER FUNCTION run_cmd(cmd)
      CHARACTER(LEN=*), INTENT(IN) :: cmd
      run_cmd=run(F_C_STRING_FUNC(cmd))
   END FUNCTION run_cmd
   
   INTEGER FUNCTION set(name,val)
      CHARACTER(LEN=*), INTENT(IN) :: name
      INTEGER, INTENT(IN) :: val
      set=setp(F_C_STRING_FUNC(name),val)
   END FUNCTION set   
   
   INTEGER FUNCTION load_mod(name)
      CHARACTER(LEN=*), INTENT(IN) :: name
      load_mod=load_module(F_C_STRING_FUNC(name))
   END FUNCTION load_mod
   
END MODULE py2f


PROGRAM main
   use py2f
   IMPLICIT NONE
   INTEGER :: x
   
   x=setup()
   x=load_mod("numpy")
  ! x=set('x',1)
!    x=run_cmd('from time import time,ctime')
!    x=run_cmd('print ctime(time())')
   x=run_cmd('numpy.__version__')
   
   x=finish()
   
END PROGRAM main
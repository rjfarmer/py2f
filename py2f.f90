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
      INTEGER(C_INT) FUNCTION load_module(name) BIND(C,NAME='c_load_module')
      USE ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name
      END FUNCTION load_module
   END INTERFACE
   
   
   INTERFACE
      INTEGER(C_INT) FUNCTION get_string(m,name,str) BIND(C,NAME='c_get_str')
      USE ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: m,name
      TYPE(c_ptr),intent(out) :: str
      END FUNCTION get_string
   END INTERFACE
   
   INTERFACE
      INTEGER(C_INT) FUNCTION get_integer(m,name,val) BIND(C,NAME='c_get_int')
      USE ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: m,name
      INTEGER(C_LONG),intent(out) :: val
      END FUNCTION get_integer
   END INTERFACE
   
    INTERFACE
      INTEGER(C_INT) FUNCTION set_integer(m,name,val) BIND(C,NAME='c_set_int')
      USE ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: m,name
      INTEGER(C_LONG),intent(in) :: val
      END FUNCTION set_integer
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

   SUBROUTINE C_F_STRING_FUNC (C_STRING_PTR,F_STRING,length)
      USE :: ISO_C_BINDING
      IMPLICIT NONE
      CHARACTER(LEN=*,kind=c_char), INTENT(OUT) :: F_STRING
      TYPE(c_ptr),target,intent(in)        :: C_STRING_PTR
      INTEGER(kind=c_int)                     :: N, l
      INTEGER,intent(out)                      :: length 
      CHARACTER,POINTER,DIMENSION(:) :: tmp=>null()
      
      N = LEN_TRIM(F_STRING)      
      call c_f_pointer(c_string_ptr,tmp,[N])
      
      l=0
      do while(tmp(l+1) /= c_null_char)
         l=l+1
      end do
      
      l=min(l,N)
      F_string=transfer(tmp(1:l),f_string)
      length=l

   END SUBROUTINE C_F_STRING_FUNC   
   
   
   INTEGER FUNCTION run_cmd(cmd)
      CHARACTER(LEN=*), INTENT(IN) :: cmd
      run_cmd=run(F_C_STRING_FUNC(cmd))
   END FUNCTION run_cmd
   
   INTEGER FUNCTION load_mod(name)
      CHARACTER(LEN=*), INTENT(IN) :: name
      load_mod=load_module(F_C_STRING_FUNC(name))
   END FUNCTION load_mod
   
   INTEGER FUNCTION get_str(m,name,value,length)
      USE ISO_C_BINDING

      CHARACTER(LEN=*), INTENT(IN) :: m,name
      TYPE(c_ptr) :: cstr
      CHARACTER(len=*),intent(out) :: value
      integer, intent(out) :: length
      
      get_str=get_string(F_C_STRING_FUNC(m),F_C_STRING_FUNC(name),cstr)
      
      call C_F_STRING_FUNC(cstr,value,length)
      
   END FUNCTION get_str
   
   INTEGER FUNCTION get_int(m,name,val)
      CHARACTER(len=*),intent(in) :: m,name
      INTEGER(8), intent(out) :: val
      
      get_int=get_integer(F_C_STRING_FUNC(m),name,val)
   
   END FUNCTION get_int
   
   INTEGER FUNCTION set_int(m,name,val)
      CHARACTER(len=*),intent(in) :: m,name
      INTEGER(8), intent(in) :: val
      
      set_int=set_integer(F_C_STRING_FUNC(m),name,val)
   
   END FUNCTION set_int
   
END MODULE py2f


PROGRAM main
   use py2f
   IMPLICIT NONE
   INTEGER :: x,res,length,res1,res2
   INTEGER(8) :: y
   INTEGER(8) :: xx=5
   CHARACTER(len=256) :: s
   
   x=setup()
   x=load_mod("numpy")
   
   res=get_str("numpy","__version__",s,length)
   write(*,*) res,s(1:length)
   
   res1=set_int('',"xx",xx)
   res2=get_int('',"xx",y)
   write(*,*) y,res1,res2

   
   x=finish()
   
END PROGRAM main
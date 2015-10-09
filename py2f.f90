MODULE py2f
   IMPLICIT NONE
   
   CHARACTER(len=*),PARAMETER :: MAIN_MOD="__main__"
   INTEGER,PARAMETER :: SUCCESS=0,FAILURE=-1
   
   INTERFACE
      INTEGER(C_INT) FUNCTION setup() BIND(C,NAME='c_setup')
      USE, INTRINSIC :: ISO_C_BINDING
      END FUNCTION setup
   END INTERFACE
   
   INTERFACE
      INTEGER(C_INT) FUNCTION finish() BIND(C,NAME='c_finish')
      USE, INTRINSIC :: ISO_C_BINDING
      END FUNCTION finish
   END INTERFACE
   
   INTERFACE
      INTEGER(C_INT) FUNCTION run(cmd) BIND(C,NAME='c_run')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: cmd
      END FUNCTION run
   END INTERFACE

   INTERFACE
      INTEGER(C_INT) FUNCTION load_module(name) BIND(C,NAME='c_load_module')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name
      END FUNCTION load_module
   END INTERFACE
   
   
   INTERFACE
      INTEGER(C_INT) FUNCTION get_string(name,str) BIND(C,NAME='c_get_str')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name
      TYPE(c_ptr),intent(out) :: str
      END FUNCTION get_string
   END INTERFACE
   
   INTERFACE
      INTEGER(C_INT) FUNCTION get_integer(name,val) BIND(C,NAME='c_get_int')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name
      INTEGER(C_LONG),intent(out) :: val
      END FUNCTION get_integer
   END INTERFACE
   
   INTERFACE
      INTEGER(C_INT) FUNCTION get_double(name,val) BIND(C,NAME='c_get_double')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name
      REAL(C_DOUBLE),intent(out) :: val
      END FUNCTION get_double
   END INTERFACE
   
    INTERFACE
      INTEGER(C_INT) FUNCTION set_integer(name,val) BIND(C,NAME='c_set_int')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name
      INTEGER(C_LONG),intent(in), VALUE :: val
      END FUNCTION set_integer
   END INTERFACE  

    INTERFACE
      INTEGER(C_INT) FUNCTION set_double(name,val) BIND(C,NAME='c_set_double')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name
      REAL(C_DOUBLE),intent(in), VALUE :: val
      END FUNCTION set_double
   END INTERFACE  
   
    INTERFACE
      INTEGER(C_INT) FUNCTION set_string(name,val) BIND(C,NAME='c_set_str')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: val
      END FUNCTION set_string
   END INTERFACE  
   
   
   INTERFACE set
      module procedure set_int,set_dble,set_str
   END INTERFACE 
   
   INTERFACE get
      module procedure get_int,get_dble,get_str
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
      USE, INTRINSIC :: ISO_C_BINDING
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
   
   INTEGER FUNCTION get_str(name,value,length)
      USE, INTRINSIC :: ISO_C_BINDING

      CHARACTER(LEN=*), INTENT(IN) :: name
      TYPE(c_ptr) :: cstr
      CHARACTER(len=*),intent(out) :: value
      integer, intent(out) :: length
      
      get_str=get_string(F_C_STRING_FUNC(name),cstr)
      
      call C_F_STRING_FUNC(cstr,value,length)
      
   END FUNCTION get_str
   
   INTEGER FUNCTION get_int(name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name
      INTEGER(C_LONG), intent(out) :: val
      
      get_int=get_integer(F_C_STRING_FUNC(name),val)
   
   END FUNCTION get_int
   
   INTEGER FUNCTION get_dble(name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name
      REAL(C_DOUBLE), intent(out) :: val
      
      get_dble=get_double(F_C_STRING_FUNC(name),val)
   
   END FUNCTION get_dble
   
   INTEGER FUNCTION set_int(name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name
      INTEGER(C_LONG), intent(in), VALUE :: val
      set_int=set_integer(F_C_STRING_FUNC(name),val)
   
   END FUNCTION set_int
   
   INTEGER FUNCTION set_dble(name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name
      REAL(C_DOUBLE), intent(in), VALUE :: val
      set_dble=set_double(F_C_STRING_FUNC(name),val)
   
   END FUNCTION set_dble
   
   INTEGER FUNCTION set_str(name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name
      CHARACTER(len=*),intent(in) :: val
      set_str=set_string(F_C_STRING_FUNC(name),F_C_STRING_FUNC(val))
   
   END FUNCTION set_str
   
END MODULE py2f


PROGRAM main
   use py2f
   IMPLICIT NONE
   INTEGER :: x,res,length,res1,res2
   INTEGER(8) :: y
   INTEGER(8) :: xx=1
   CHARACTER(len=256) :: s
   DOUBLE PRECISION :: aa,bb
   
   aa=10.d0
   
   res=setup()
   if(res/=SUCCESS) THEN
      write(*,*) "Init failed"
      stop
   end if
   res=load_mod("numpy")
   if(res/=SUCCESS) THEN
      write(*,*) "Load failed"
      stop
   end if
   !x=run_cmd("import numpy as np")
   res=set("zz","abc")
    write(*,*) res
    if(res/=SUCCESS) stop
   res=get("zz",s,length)
    write(*,*) res,s(1:length)
    if(res/=SUCCESS) stop
   
   res1=set("xx",xx)
   write(*,*) "res1",res1
   if(res1/=SUCCESS) stop
   
   res2=get("xx",y)
   if(res2/=SUCCESS) stop
   write(*,*) y,res1,res2

   res1=set("yy",aa)
   if(res1/=SUCCESS) stop
   
   res2=get("yy",bb)
   if(res2/=SUCCESS) stop
    write(*,*) bb,res1,res2   
   
   
   x=finish()
   
END PROGRAM main
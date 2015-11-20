MODULE py2f
   IMPLICIT NONE
   
   CHARACTER(len=*),PARAMETER :: MAIN_MOD=""
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

!    INTERFACE
!       INTEGER(C_INT) FUNCTION load_module(name) BIND(C,NAME='c_load_module')
!       USE, INTRINSIC :: ISO_C_BINDING
!       CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name
!       END FUNCTION load_module
!    END INTERFACE
!    
   
   INTERFACE
      INTEGER(C_INT) FUNCTION get_string(obj,name,str) BIND(C,NAME='c_get_str')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name,obj
      TYPE(c_ptr),intent(out) :: str
      END FUNCTION get_string
   END INTERFACE
   
   INTERFACE
      INTEGER(C_INT) FUNCTION get_integer(obj,name,val) BIND(C,NAME='c_get_int')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name,obj
      INTEGER(C_LONG),intent(out) :: val
      END FUNCTION get_integer
   END INTERFACE
   
   INTERFACE
      INTEGER(C_INT) FUNCTION get_double(obj,name,val) BIND(C,NAME='c_get_double')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name,obj
      REAL(C_DOUBLE),intent(out) :: val
      END FUNCTION get_double
   END INTERFACE
   
    INTERFACE
      INTEGER(C_INT) FUNCTION set_integer(obj,name,val) BIND(C,NAME='c_set_int')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name,obj
      INTEGER(C_LONG),intent(in), VALUE :: val
      END FUNCTION set_integer
   END INTERFACE  

    INTERFACE
      INTEGER(C_INT) FUNCTION set_double(obj,name,val) BIND(C,NAME='c_set_double')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name,obj
      REAL(C_DOUBLE),intent(in), VALUE :: val
      END FUNCTION set_double
   END INTERFACE  
   
    INTERFACE
      INTEGER(C_INT) FUNCTION set_string(obj,name,val) BIND(C,NAME='c_set_str')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name,obj
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: val
      END FUNCTION set_string
   END INTERFACE  

    INTERFACE
      INTEGER(C_INT) FUNCTION set_int_array_1d(obj,name,len_val,val) BIND(C,NAME='c_set_int_array_1d')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name,obj
      TYPE(C_PTR),intent(in),VALUE :: val
      INTEGER(C_INT),INTENT(IN),VALUE :: len_val
      END FUNCTION set_int_array_1d
   END INTERFACE  
   
    INTERFACE
      INTEGER(C_INT) FUNCTION get_int_array_1d(obj,name,val) BIND(C,NAME='c_get_int_array_1d')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name,obj
      TYPE(C_PTR),intent(inout) :: val
      END FUNCTION get_int_array_1d
   END INTERFACE 
 
    INTERFACE
      INTEGER(C_INT) FUNCTION set_double_array_1d(obj,name,len_val,val) BIND(C,NAME='c_set_double_array_1d')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name,obj
      TYPE(C_PTR),intent(in),VALUE :: val
      INTEGER(C_INT),INTENT(IN),VALUE :: len_val
      END FUNCTION set_double_array_1d
   END INTERFACE  
   
    INTERFACE
      INTEGER(C_INT) FUNCTION get_double_array_1d(obj,name,val) BIND(C,NAME='c_get_double_array_1d')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name,obj
      TYPE(C_PTR),intent(inout) :: val
      END FUNCTION get_double_array_1d
   END INTERFACE 
   
    INTERFACE
      INTEGER(C_INT) FUNCTION get_array_len(obj,name) BIND(C,NAME='_get_array_len')
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=1,kind=C_char),dimension(*),intent(in) :: name,obj
      END FUNCTION get_array_len
   END INTERFACE 
   
   INTERFACE set
      module procedure set_int,set_dble,set_str,set_dble_arr_1d,set_int_arr_1d
   END INTERFACE 
   
!    INTERFACE set_arr_1d
!       module procedure set_dble_arr_1d
!    END INTERFACE 
   
   INTERFACE get
      module procedure get_int,get_dble,get_str,get_dble_arr_1d,get_int_arr_1d
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

   SUBROUTINE C_F_STRING_FUNC (C_STRING_PTR,F_STRING)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      CHARACTER(LEN=*,kind=c_char), INTENT(OUT) :: F_STRING
      TYPE(c_ptr),target,intent(in)        :: C_STRING_PTR
      INTEGER(kind=c_int)                     :: N, l
      CHARACTER,POINTER,DIMENSION(:) :: tmp=>null()
      
      N = LEN_TRIM(F_STRING)      
      call c_f_pointer(c_string_ptr,tmp,[N])
      
      l=0
      do 
         if (tmp(l+1) == c_null_char) exit
         l=l+1
      end do
      
      l=min(l,N)
      F_string=transfer(tmp(1:l),f_string)
      F_STRING(l+1:N)=''

   END SUBROUTINE C_F_STRING_FUNC   
   
   
   INTEGER FUNCTION run_cmd(cmd)
      CHARACTER(LEN=*), INTENT(IN) :: cmd
      run_cmd=run(F_C_STRING_FUNC(cmd))
   END FUNCTION run_cmd
   
!    INTEGER FUNCTION load_mod(name)
!       CHARACTER(LEN=*), INTENT(IN) :: name
!       load_mod=load_module(F_C_STRING_FUNC(name))
!    END FUNCTION load_mod
   
   INTEGER FUNCTION get_str(obj,name,value)
      USE, INTRINSIC :: ISO_C_BINDING

      CHARACTER(LEN=*), INTENT(IN) :: name,obj
      TYPE(c_ptr) :: cstr
      CHARACTER(len=*),intent(out) :: value
      
      get_str=get_string(F_C_STRING_FUNC(obj),F_C_STRING_FUNC(name),cstr)
      
      call C_F_STRING_FUNC(cstr,value)
      
   END FUNCTION get_str
   
   INTEGER FUNCTION get_int(obj,name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name,obj
      INTEGER(C_LONG), intent(out) :: val
      
      get_int=get_integer(F_C_STRING_FUNC(obj),F_C_STRING_FUNC(name),val)
   
   END FUNCTION get_int
   
   INTEGER FUNCTION get_dble(obj,name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name,obj
      REAL(C_DOUBLE), intent(out) :: val
      
      get_dble=get_double(F_C_STRING_FUNC(obj),F_C_STRING_FUNC(name),val)
   
   END FUNCTION get_dble
   
   INTEGER FUNCTION set_int(obj,name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name,obj
      INTEGER(C_LONG), intent(in), VALUE :: val
      set_int=set_integer(F_C_STRING_FUNC(obj),F_C_STRING_FUNC(name),val)
   
   END FUNCTION set_int
   
   INTEGER FUNCTION set_dble(obj,name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name,obj
      REAL(C_DOUBLE), intent(in), VALUE :: val
      set_dble=set_double(F_C_STRING_FUNC(obj),F_C_STRING_FUNC(name),val)
   
   END FUNCTION set_dble
   
   INTEGER FUNCTION set_str(obj,name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name,obj
      CHARACTER(len=*),intent(in) :: val
      set_str=set_string(F_C_STRING_FUNC(obj),F_C_STRING_FUNC(name),F_C_STRING_FUNC(val))
   
   END FUNCTION set_str
   
   INTEGER FUNCTION set_dble_arr_1d(obj,name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name,obj
      REAL(C_DOUBLE), intent(in),dimension(:),target :: val
      INTEGER(C_INT) :: len_val
      len_val=size(val,kind=C_INT)
      
      set_dble_arr_1d=set_double_array_1d(F_C_STRING_FUNC(obj),F_C_STRING_FUNC(name),len_val,C_LOC(val))
   
   END FUNCTION set_dble_arr_1d
   
   INTEGER FUNCTION get_dble_arr_1d(obj,name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name,obj
      REAL(C_DOUBLE),intent(inout), allocatable,dimension(:),target :: val
      REAL(C_DOUBLE),pointer,dimension(:) :: val_ptr
      TYPE(c_ptr) :: array_in
      INTEGER(C_INT) :: arrlen
      
      !Get size of array
      arrlen=get_array_len(F_C_STRING_FUNC(obj),F_C_STRING_FUNC(name))
      allocate(val(arrlen))
      val=0.d0
      
      get_dble_arr_1d=get_double_array_1d(F_C_STRING_FUNC(obj),F_C_STRING_FUNC(name),array_in)
   
       call  C_F_POINTER(array_in,val_ptr,[arrlen])
      
      val(1:arrlen)=val_ptr(1:arrlen)
   
   END FUNCTION get_dble_arr_1d
   

   INTEGER FUNCTION set_int_arr_1d(obj,name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name,obj
      INTEGER(C_INT), intent(in),dimension(:),target :: val
      INTEGER(C_INT) :: len_val
      len_val=size(val,kind=C_INT)
      
      set_int_arr_1d=set_int_array_1d(F_C_STRING_FUNC(obj),F_C_STRING_FUNC(name),len_val,C_LOC(val))
   
   END FUNCTION set_int_arr_1d
   
   INTEGER FUNCTION get_int_arr_1d(obj,name,val)
      USE, INTRINSIC :: ISO_C_BINDING
      CHARACTER(len=*),intent(in) :: name,obj
      INTEGER(C_INT),intent(inout), allocatable,dimension(:),target :: val
      INTEGER(C_INT),pointer,dimension(:) :: val_ptr
      TYPE(c_ptr) :: array_in
      INTEGER(C_INT) :: arrlen
      
      !Get size of array
      arrlen=get_array_len(F_C_STRING_FUNC(obj),F_C_STRING_FUNC(name))
      allocate(val(arrlen))
      val=0
      
      
      get_int_arr_1d=get_int_array_1d(F_C_STRING_FUNC(obj),F_C_STRING_FUNC(name),array_in)
   
       call  C_F_POINTER(array_in,val_ptr,[arrlen])
      
      val(1:arrlen)=val_ptr(1:arrlen)
         
   END FUNCTION get_int_arr_1d
   
END MODULE py2f

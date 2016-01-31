MODULE tester
   use py2f
   IMPLICIT NONE
   
   CONTAINS
   
   SUBROUTINE init()
      IMPLICIT NONE
      
       CALL init_random_seed() 
   
   END SUBROUTINE init
   
! From https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html#RANDOM_005fSEED
   subroutine init_random_seed()
      use iso_fortran_env, only: int64
      implicit none
      integer, allocatable :: seed(:)
      integer :: i, n, un, istat, dt(8), pid
      integer(int64) :: t

      call random_seed(size = n)
      allocate(seed(n))
      ! First try if the OS provides a random number generator
      open(newunit=un, file="/dev/urandom", access="stream", &
         form="unformatted", action="read", status="old", iostat=istat)
      if (istat == 0) then
         read(un) seed
         close(un)
      else
         ! Fallback to XOR:ing the current time and pid. The PID is
         ! useful in case one launches multiple instances of the same
         ! program in parallel.
         call system_clock(t)
         if (t == 0) then
            call date_and_time(values=dt)
            t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
                  + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
                  + dt(3) * 24_int64 * 60 * 60 * 1000 &
                  + dt(5) * 60 * 60 * 1000 &
                  + dt(6) * 60 * 1000 + dt(7) * 1000 &
                  + dt(8)
         end if
         pid = getpid()
         t = ieor(t, int(pid, kind(t)))
         do i = 1, n
            seed(i) = lcg(t)
         end do
      end if
      call random_seed(put=seed)
      contains
         ! This simple PRNG might not be good enough for real work, but is
         ! sufficient for seeding a better PRNG.
         function lcg(s)
            integer :: lcg
            integer(int64) :: s
            if (s == 0) then
               s = 104729
            else
               s = mod(s, 4294967296_int64)
            end if
            s = mod(s * 279470273_int64, 4294967291_int64)
            lcg = int(mod(s, int(huge(0), int64)), kind(0))
         end function lcg
   end subroutine init_random_seed
   
   SUBROUTINE random_int(x)
      INTEGER(4),INTENT(INOUT),DIMENSION(:) :: x
      REAL :: r
      INTEGER :: i
      integer,parameter :: seed = 86456
          
      call srand(seed)
      do i=1,size(x)
         call RANDOM_NUMBER(r)
         x(i)=int(HUGE(x(i))*r)
      end do

   END SUBROUTINE random_int
   
   SUBROUTINE check(res,func,critical)
      IMPLICIT NONE
      INTEGER :: res
      CHARACTER(LEN=*) :: func
      !Should we continue if this test fails?
      LOGICAL, OPTIONAL :: critical
      write(*,'(A)',advance='no') "Testing "//func//" "
      if(res==SUCCESS)THEN
         write(*,*) "Passed"
      else if(res==NOT_IMPLEMENTED) THEN
         write(*,*) "Not Implemeneted, Passing"
      else
         write(*,*) "Failed"
         if(present(critical))then
            if(critical)then
               stop
            end if
         end if
      end if
   END SUBROUTINE check

   SUBROUTINE test_setup()
      IMPLICIT NONE

      call check(setup(),"setup",.TRUE.)
         
   END SUBROUTINE test_setup
 
   SUBROUTINE test_finish()
      IMPLICIT NONE

      call check(finish(),"finish",.TRUE.)
         
   END SUBROUTINE test_finish
   
   SUBROUTINE test_int()
      IMPLICIT NONE
      INTEGER(4) :: res_4
      INTEGER(8) :: res_8

      call check(set(MAIN_MOD,"x_int4",int(1,kind=4)),"set_int4")
      call check(get(MAIN_MOD,"x_int4",res_4),"get_int4")
      if(res_4/=int(1,kind=4)) call check(FAILURE,"set_int4 does not match get_int4")
      
      call check(set(MAIN_MOD,"x_int8",int(1,kind=8)),"set_int8")
      call check(get(MAIN_MOD,"x_int8",res_8),"get_int8")
      if(res_8/=int(1,kind=8)) call check(FAILURE,"set_int8 does not match get_int8")
      
   END SUBROUTINE test_int  
   
   SUBROUTINE test_int_arr()
      IMPLICIT NONE
      INTEGER, PARAMETER ::arrSize=5
      INTEGER(4),DIMENSION(arrSize) :: arrInS1
      INTEGER(4),DIMENSION(arrSize*arrSize) :: arrInS2
      INTEGER(4),DIMENSION(arrSize*arrSize*arrSize) :: arrInS3
      INTEGER(4),DIMENSION(arrSize*arrSize*arrSize*arrSize) :: arrInS4
      INTEGER(4),DIMENSION(arrSize*arrSize*arrSize*arrSize*arrSize) :: arrInS5
      
      INTEGER(4),DIMENSION(arrSize) :: arrIn1
      INTEGER(4),DIMENSION(arrSize,arrSize) :: arrIn2
      INTEGER(4),DIMENSION(arrSize,arrSize,arrSize) :: arrIn3
      INTEGER(4),DIMENSION(arrSize,arrSize,arrSize,arrSize) :: arrIn4
      INTEGER(4),DIMENSION(arrSize,arrSize,arrSize,arrSize,arrSize) :: arrIn5
      
      INTEGER(4),DIMENSION(:),ALLOCATABLE :: arrOut1
      INTEGER(4),DIMENSION(:,:),ALLOCATABLE :: arrOut2
      INTEGER(4),DIMENSION(:,:,:),ALLOCATABLE :: arrOut3
      INTEGER(4),DIMENSION(:,:,:,:),ALLOCATABLE :: arrOut4
      INTEGER(4),DIMENSION(:,:,:,:,:),ALLOCATABLE :: arrOut5
      
      
      INTEGER :: i,j,k,l,m
      
      call random_int(arrInS1)
      call random_int(arrInS2)
      call random_int(arrInS3)
      call random_int(arrInS4)
      call random_int(arrInS5)
      
      arrIn1=arrInS1
      arrIn2=reshape(arrInS2,(/arrSize,arrSize/))
      arrIn3=reshape(arrInS3,(/arrSize,arrSize,arrSize/))
      arrIn4=reshape(arrInS4,(/arrSize,arrSize,arrSize,arrSize/))
      arrIn5=reshape(arrInS5,(/arrSize,arrSize,arrSize,arrSize,arrSize/))
         
      call check(set(MAIN_MOD,"x_int4Arr_1d",arrIn1),"set_int4Arr_1d")
      call check(get(MAIN_MOD,"x_int4Arr_1d",arrOut1),"get_int4Arr_1d")
      
      do i=1,arrSize
         if(arrIn1(i)/=arrOut1(i)) then
            call check(FAILURE,"set_int4Arr_1d does not match get_int4Arr_1d")
            exit
         end if
      end do
      
      call check(set(MAIN_MOD,"x_int4Arr_2d",arrIn2),"set_int4Arr_2d")
      call check(get(MAIN_MOD,"x_int4Arr_2d",arrOut2),"get_int4Arr_2d")
      
      do i=1,arrSize
         do j=1,arrSize
            if(arrIn2(i,j)/=arrOut2(i,j)) then
               call check(NOT_IMPLEMENTED,"set_int4Arr_2d does not match get_int4Arr_2d")
               exit
            end if
         end do
      end do
      
      call check(set(MAIN_MOD,"x_int4Arr_3d",arrIn3),"set_int4Arr_3d")
      call check(get(MAIN_MOD,"x_int4Arr_3d",arrOut3),"get_int4Arr_3d")
      
      do i=1,arrSize
         do j=1,arrSize
            do k=1,arrSize
               if(arrIn3(i,j,k)/=arrOut3(i,j,k)) then
                  call check(NOT_IMPLEMENTED,"set_int4Arr_3d does not match get_int4Arr_3d")
                  exit
               end if
            end do
         end do
      end do
      
      call check(set(MAIN_MOD,"x_int4Arr_4d",arrIn4),"set_int4Arr_4d")
      call check(get(MAIN_MOD,"x_int4Arr_4d",arrOut4),"get_int4Arr_4d")
      
      do i=1,arrSize
         do j=1,arrSize
            do k=1,arrSize
               do l=1,arrSize
                  if(arrIn4(i,j,k,l)/=arrOut4(i,j,k,l)) then
                     call check(NOT_IMPLEMENTED,"set_int4Arr_4d does not match get_int4Arr_4d")
                     exit
                  end if
               end do
            end do
         end do
      end do
      
      call check(set(MAIN_MOD,"x_int4Arr_5d",arrIn5),"set_int4Arr_5d")
      call check(get(MAIN_MOD,"x_int4Arr_5d",arrOut5),"get_int4Arr_5d")
      
      do i=1,arrSize
         do j=1,arrSize
            do k=1,arrSize
               do l=1,arrSize
                  do m=1,arrSize
                     if(arrIn5(i,j,k,l,m)/=arrOut5(i,j,k,l,m)) then
                        call check(NOT_IMPLEMENTED,"set_int4Arr_5d does not match get_int4Arr_5d")
                        exit
                     end if
                  end do
               end do
            end do
         end do
      end do
      
      
      
      
   END SUBROUTINE test_int_arr
   
   
   SUBROUTINE test_dble()
      IMPLICIT NONE

      call check(set(MAIN_MOD,"x_dble",dble(1.d0)),"set_dble")
      call check(set(MAIN_MOD,"x_float",real(1.0)),"set_float")
         
   END SUBROUTINE test_dble 
   
   SUBROUTINE test_str()
      IMPLICIT NONE
      CHARACTER(len=256) :: out

      call check(set(MAIN_MOD,"x_str","1"),"set_str")
      call check(get(MAIN_MOD,"x_str",out),"get_str")
      if(trim(out) .ne. "1") call check(FAILURE,"set_str does not match get_str")
         
   END SUBROUTINE test_str  

END MODULE tester



PROGRAM main
   use tester
   IMPLICIT NONE
   
   !Routines needed setup but we are not testing
   call init()
   
   call test_setup()
   
   !Single values
   call test_int()
   call test_dble()
   call test_str()
   
   !Arrays
   call test_int_arr()
   
   call test_finish()

END PROGRAM main

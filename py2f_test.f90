MODULE tester
   use py2f
   IMPLICIT NONE
   
   CONTAINS
   
   SUBROUTINE check(res,func,critical)
      IMPLICIT NONE
      INTEGER :: res
      CHARACTER(LEN=*) :: func
      !Should we continue if this test fails?
      LOGICAL, OPTIONAL :: critical
      write(*,'(A)',advance='no') "Testing "//func//" "
      if(res==SUCCESS)THEN
         write(*,*) "Passed"
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
   
   SUBROUTINE test_set_int()
      IMPLICIT NONE
      INTEGER(4) :: res_4
      INTEGER(8) :: res_8

      call check(set(MAIN_MOD,"x_int4",int(1,kind=4)),"set_int4")
      call check(get(MAIN_MOD,"x_int4",res_4),"get_int4")
      if(res_4/=int(1,kind=4)) call check(FAILURE,"set_int4 does not match get_int4")
      
      call check(set(MAIN_MOD,"x_int8",int(1,kind=8)),"set_int8")
      call check(get(MAIN_MOD,"x_int8",res_8),"get_int8")
      if(res_8/=int(1,kind=8)) call check(FAILURE,"set_int8 does not match get_int8")
      
   END SUBROUTINE test_set_int  
   
   SUBROUTINE test_set_dble()
      IMPLICIT NONE

      call check(set(MAIN_MOD,"x_dble",dble(1.d0)),"set_dble")
      call check(set(MAIN_MOD,"x_float",real(1.0)),"set_float")
         
   END SUBROUTINE test_set_dble 
   
   SUBROUTINE test_set_str()
      IMPLICIT NONE

      call check(set(MAIN_MOD,"x_str","1"),"set_str")
         
   END SUBROUTINE test_set_str  

END MODULE tester



PROGRAM main
   use tester
   IMPLICIT NONE
   
   call test_setup()
   
   !Single values
   call test_set_int()
   call test_set_dble()
   call test_set_str()
   
   call test_finish()

END PROGRAM main

MODULE tester
   use py2f
   IMPLICIT NONE
   
   CONTAINS
   
   SUBROUTINE check(res,func)
      IMPLICIT NONE
      INTEGER :: res
      CHARACTER(LEN=*) :: func
      write(*,'(A)',advance='no') "Testing "//func//" "
      if(res==SUCCESS)THEN
         write(*,*) "Passed"
      else
         write(*,*) "Failed"
         stop
      end if
   END SUBROUTINE check

   SUBROUTINE test_setup()
      IMPLICIT NONE

      call check(setup(),"setup")
         
   END SUBROUTINE test_setup
 
   SUBROUTINE test_finish()
      IMPLICIT NONE

      call check(finish(),"finish")
         
   END SUBROUTINE test_finish
   
   SUBROUTINE test_set_int()
      IMPLICIT NONE

      call check(set(MAIN_MOD,"x",1),"set_int")
         
   END SUBROUTINE test_set_int  
   
   SUBROUTINE test_set_dble()
      IMPLICIT NONE

      call check(set(MAIN_MOD,"x",1.d0),"set_dble")
         
   END SUBROUTINE test_set_dble 
   
   SUBROUTINE test_set_str()
      IMPLICIT NONE

      call check(set(MAIN_MOD,"x","1"),"set_str")
         
   END SUBROUTINE test_set_str  
   

END MODULE tester



PROGRAM main
   use tester
   IMPLICIT NONE
   
   call test_setup()
   call test_set_int()
   call test_set_dble()
   call test_set_str()
   
   call test_finish()

END PROGRAM main


PROGRAM main
   use py2f
   IMPLICIT NONE
   INTEGER :: x,res,res1,res2
   INTEGER(8) :: y
   INTEGER(8) :: xx=1
   CHARACTER(len=256) :: s
   DOUBLE PRECISION :: aa,bb
   DOUBLE PRECISION,DIMENSION(1:3) :: xarr
   
   aa=10.d0
   
   res=setup()
   if(res/=SUCCESS) THEN
      write(*,*) "Init failed"
      stop
   end if
   res=run_cmd("import numpy")
   if(res/=SUCCESS) THEN
      write(*,*) "Load failed"
      stop
   end if
   !x=run_cmd("import numpy as np")
   res=set(MAIN_MOD,"zz","abc")
    write(*,*) res
    if(res/=SUCCESS) stop
    
   res=get("numpy","__version__",s)
    write(*,*) res,trim(s)
    if(res/=SUCCESS) stop
   
   res1=set("numpy","xxxxxxxxxxxx",xx)
   write(*,*) "res1",res1
   if(res1/=SUCCESS) stop
   
   res2=get("numpy","xxxxxxxxxxxx",y)
   if(res2/=SUCCESS) stop
   write(*,*) "**",y,res1,res2
   
   res1=set(MAIN_MOD,"yy",aa)
   if(res1/=SUCCESS) stop
   
   res2=get(MAIN_MOD,"yy",bb)
   if(res2/=SUCCESS) stop
   write(*,*) bb,res1,res2   
   
!    xarr=(/1.d0,2.d0,3.d0/)
!    res2=set_arr_1d(MAIN_MOD,"xarr",xarr)
!    if(res2/=SUCCESS) stop   
   

   x=finish()
   
END PROGRAM main

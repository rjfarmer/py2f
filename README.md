# py22f

Allows embedding python code inside fortran. 

# Installation
make

# Using
All functions retun an int error code, check agaisnt the SUCCESS 
or FAILURE constants.

````fortran
import py2f
````
Import modules

````fortran
   res=setup()
   if(res/=SUCCESS) THEN
      write(*,*) "Init failed"
      stop
   end if
````
Initilises the code must be called before any other py2f function

````fortran
res=finish()
````
Shutdown python at the end, not much to do if it fails

````fortran
res=set(str object, str name, int|double|str value)
````
Creates a parmeter called name with value value. If
you want to create the variable in the top level python namespace
ie if you typed
````python
x=1
````
in the python interpreter, then object=MAINMOD, otherwise pass
the name of the pyython object as a string.


````fortran
res=get(str object, str name, int|double|str value)
````
Gets a parmeter called name with value value. If
the variable is in the top level python namespace, then object=MAINMOD, otherwise pass
the name of the pyython object as a string.

Note if you are getting a string then there is an extra argument
````fortran
res=get(str object, str name, str value, int length)
````
whichh returns the length of the string, anything pass length is
garbage.




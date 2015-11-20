# py2f

Allows embedding python code inside fortran. 

# Installation
make

# Using
All functions return an int error code, check against the SUCCESS 
or FAILURE constants.

## Startup
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

Initializes the code must be called before any other py2f function

## End

````fortran
res=finish()
````

Shutdown python at the end, not much to do if it fails

## Set a variable

````fortran
res=set(str object, str name, int|double|str value)
````
Creates a parameter called name with value value. If
you want to create the variable in the top level python name space
ie if you typed

````python
x=1
````

in the python interpreter, then object=MAINMOD, 

````fortran
res=set(MAINMOD,"x",1)
````

otherwise pass
the name of the python object as a string.

Can also pass 1d int or double arrays to set, note we pass the pointer,
so be careful that after deallocating the array, you dont try and use it in python.

## Get a variable

````fortran
res=get(str object, str name, int|double|str value)
````

Gets a parameter called name with value value. If
the variable is in the top level python name space, then object=MAINMOD, otherwise pass
the name of the python object as a string.

Can also pass 1d int or double arrays to get, which will return a copy of the array.


# py22f

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

in the python interpreter, then object=MAINMOD, otherwise pass
the name of the python object as a string.

## Get a variable

````fortran
res=get(str object, str name, int|double|str value)
````

Gets a parameter called name with value value. If
the variable is in the top level python name space, then object=MAINMOD, otherwise pass
the name of the python object as a string.

Note if you are getting a string then there is an extra argument

````fortran
res=get(str object, str name, str value, int length)
````
which returns the length of the string, anything passed length is
garbage.



#include <Python.h>
#include "py2f.h"

#define SUCCESS 0
#define FAIL -1 
#define MAX_STR_LEN 256

PyObject *x;

typedef struct{
  PyObject* m;
  char name[MAX_STR_LEN];
} module;
  
typedef struct {
 module mod[100];
 int c;  
} loaded_modules;

loaded_modules lmod;

int c_setup()
{
  Py_SetProgramName("python"); 
  Py_Initialize();
  
  lmod.c=0;
  
  return SUCCESS;
}

int c_finish()
{
   Py_Finalize();
   return SUCCESS;
}

int c_run(const char *cmd)
{
   PyRun_SimpleString(cmd);
   return SUCCESS;
}

int c_load_module(const char *name)
{
   PyObject *pName;
   pName = PyString_FromString(name);
   char *attr;
    /* Error checking of pName left out */

    lmod.mod[lmod.c].m = PyImport_Import(pName);
    strncpy(lmod.mod[lmod.c].name,name,MAX_STR_LEN);
    lmod.c++;
    Py_DECREF(pName);

    PyObject *obj=PyObject_GetAttr(lmod.mod[0].m,PyString_FromString("__version__"));
    attr=PyString_AsString(obj);
    Py_DECREF(obj);
    
    printf("%s\n",attr);
    
   return SUCCESS;
}

int c_get_str(const char *modname, const char *name, char *value)
{
   PyObject *pName;
   pName = PyString_FromString(name);
   PyObject *obj=PyObject_GetAttr(modname,pName);
   value=PyString_AsString(obj);
   Py_DECREF(obj);
   
   return SUCCESS;
}
   
   
int c_set_int(const char *name, const int val)
{   
   x=PyInt_FromLong(val);
   Py_DECREF(x);
   return SUCCESS;
}
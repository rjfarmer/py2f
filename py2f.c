#include <Python.h>
#include "py2f.h"

#define SUCCESS 0
#define FAILURE -1 
#define MAX_STR_LEN 256
#define MAX_MODULES 100

PyObject *x;

typedef struct{
  PyObject* m;
  char name[MAX_STR_LEN];
} module;
  
typedef struct {
 module mod[MAX_MODULES];
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
    /* Error checking of pName left out */

    lmod.mod[lmod.c].m = PyImport_Import(pName);
    strncpy(lmod.mod[lmod.c].name,name,MAX_STR_LEN);
    lmod.c++;
    Py_DECREF(pName);
    
   return SUCCESS;
}


int get_mod_num(const char *modname, int *res)
{
   int i;
   signed int val=-1;
   
   for(i=0;i<=lmod.c;i++){
      if(strncmp(modname,lmod.mod[i].name,MAX_STR_LEN)==0){
         val=i;
         break;
      }
   }
   
   if(val==-1) 
      return FAILURE;
   
   *res=val;
   return SUCCESS;
}

int c_get_str(const char *modname, const char *name, char **value)
{
   PyObject *pName;
   signed int modNum,err;
   
   err=get_mod_num(modname,&modNum);
   if(err==FAILURE)
      return FAILURE;
   
   pName = PyString_FromString(name);
   
   PyObject *obj=PyObject_GetAttr(lmod.mod[modNum].m,pName);
   *value=PyString_AsString(obj);
   
   Py_DECREF(obj);
   Py_DECREF(pName);
   
   return SUCCESS;
}
   
   
int c_set_int(const char *name, const int val)
{   
   x=PyInt_FromLong(val);
   Py_DECREF(x);
   return SUCCESS;
}
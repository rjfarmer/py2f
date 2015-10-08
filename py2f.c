#include "py2f.h"
#include <stdio.h>


#define SUCCESS 0
#define FAILURE -1 
#define MAX_STR_LEN 256
#define MAX_MODULES 100

PyObject *mainmod;

int c_setup()
{
  Py_SetProgramName("python"); 
  Py_Initialize();
  
  mainmod=PyImport_AddModule("__main__");
  
  return SUCCESS;
}

int c_finish()
{
   Py_DECREF(mainmod);
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
   
   PyImport_Import(pName);

   Py_DECREF(pName);

   return SUCCESS;
}


int c_get_str(const char *module, const char *name, char **value)
{
   PyObject *obj;

   obj=_getVar(module,name);
   
   *value=PyString_AsString(obj);

   Py_DECREF(obj);
   
   return SUCCESS;
}

int c_get_int(const char *module, const char *name, long int *value)
{
   PyObject *obj;
  
   obj=_getVar(module,name);
   
   *value=PyInt_AsLong(obj);
   Py_DECREF(obj);
   
   return SUCCESS;
}


int c_set_int(const char *module, const char *name, const long int val)
{   
   int ret;
   PyObject *v;

   v=PyInt_FromLong(val);
   
   ret=PyModule_AddIntConstant(mainmod,name,val);
   
   Py_DECREF(v);
   printf("%d\n",ret);
   if (ret)
      return FAILURE;
   
   return SUCCESS;
}


int _print_dict(PyObject *dict)
{
   PyObject *key,*value;
   Py_ssize_t pos=0;
   
      while(PyDict_Next(dict,&pos,&key,&value)){
         printf("%s %s\n",PyString_AsString(key),PyString_AsString(value));
      }
      
   Py_DECREF(key);
   Py_DECREF(value);
   return SUCCESS;
}

PyObject* _getVar(const char *module, const char *name)
{
   PyObject *dict,*m,*obj;
   if(module=='\0')
   {
      obj=PyDict_GetItemString(mainmod,name);
   }else{
      m=PyImport_GetModuleDict();      
      dict=PyDict_GetItemString(m,module);
      obj=PyObject_GetAttrString(dict,name);
      Py_DECREF(dict);
      Py_DECREF(m);
   }
   
   return obj;
}

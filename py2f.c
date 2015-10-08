#include "py2f.h"
#include <stdio.h>


#define SUCCESS 0
#define FAILURE -1 
#define MAX_STR_LEN 256
#define MAX_MODULES 100

PyObject *mainmod = NULL;
PyObject *main_dict;

int c_setup()
{
  Py_SetProgramName("python"); 
  Py_Initialize();
  
  mainmod=PyImport_ImportModule("__main__");
  main_dict=PyModule_GetDict(mainmod);
  
  
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
   PyObject *m = NULL;
   int ret;

   m = PyImport_ImportModule(name);
   ret=PyDict_SetItemString(main_dict, name, m);
   
   Py_INCREF(m);

   if(ret)
      return FAILURE;
   
   return SUCCESS;
}


int c_get_str(const char *module, const char *name, char **value)
{
   PyObject *obj = NULL;

   obj=_getVar(module,name);
   
   *value=PyString_AsString(obj);

   Py_XDECREF(obj);
   
   return SUCCESS;
}

int c_get_int(const char *module, const char *name, long int *value)
{
   PyObject *obj = NULL;
  
   obj=_getVar(module,name);
   
   *value=PyInt_AsLong(obj);
   
   Py_XDECREF(obj);
   
   return SUCCESS;
}


int c_set_int(const char *module, const char *name, const int val)
{   
   PyObject *mod = NULL;

   mod=PyDict_GetItemString(main_dict,module);
   PyModule_AddIntConstant(mainmod,name,val);
 
   Py_XDECREF(mod);
   
   return SUCCESS;
}
   
   Py_DECREF(v);
   printf("%d\n",ret);
   if (ret)
      return FAILURE;
   
   return SUCCESS;
}


int _print_dict(PyObject *dict)
{
   PyObject *key = NULL;
   PyObject *value = NULL;
   Py_ssize_t pos=0;
   
      while(PyDict_Next(dict,&pos,&key,&value)){
         printf("%s %s\n",PyString_AsString(key),PyString_AsString(value));
      }
      
   Py_XDECREF(key);
   Py_XDECREF(value);
   return SUCCESS;
}

PyObject* _getVar(const char *module, const char *name)
{
   PyObject *dict = NULL;
   PyObject *m = NULL;
   PyObject *obj = NULL;

   m=PyImport_GetModuleDict();      
   dict=PyDict_GetItemString(m,module);
   obj=PyObject_GetAttrString(dict,name);
   
   Py_XDECREF(dict);
   
   return obj;
}

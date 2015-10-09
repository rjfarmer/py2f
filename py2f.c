#include "py2f.h"
#include <stdio.h>


#define SUCCESS 0
#define FAILURE -1 

#define PRINTERROR printf ("Line %d of file %s (function %s)\n",\
                      __LINE__, __FILE__, __func__)

PyObject *mainmod = NULL;
PyObject *main_dict = NULL;

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
   
   Py_DECREF(m);
   
   if(ret)
      return FAILURE;
   
   return SUCCESS;
}


int c_get_str(const char *name, char **value)
{
   PyObject *obj = NULL;

   obj=_getVar(name);
   
   if(!obj)
   {
      PRINTERROR;
      printf("%s\n",name);
      return FAILURE;
   } 
   
   *value=PyString_AsString(obj);
   
   if(!value)
   {
      PRINTERROR;
      printf("%s\n",name);
      return FAILURE;
   } 
   
   Py_XDECREF(obj);
   
   return SUCCESS;
}

int c_get_int(const char *name, long int *value)
{
   PyObject *obj = NULL;
  
   obj=_getVar(name);
   
   if(!obj)
   {
      PRINTERROR;
      printf("%s\n",name);
      return FAILURE;
   } 
   
   *value=PyInt_AsLong(obj);
   
   if(!value)
   {
      PRINTERROR;
      printf("%s\n",name);
      return FAILURE;
   } 
   
   Py_XDECREF(obj);
   
   return SUCCESS;
}

int c_get_double(const char *name, double *value)
{
   PyObject *obj = NULL;
   
   obj=_getVar(name);
   *value=PyFloat_AsDouble(obj);
   
   Py_XDECREF(obj);
   
   return SUCCESS;
}


int c_set_int(const char *name, const int val)
{   
   PyObject *v;
   int ret;
   
   v=PyInt_FromLong(1);
   
   if(!v)
   {
      PRINTERROR;
      Py_XDECREF(v);
      return FAILURE;
   }
      
   ret=_setVar(name,v);
   Py_XDECREF(v);
   
   return ret;
}


int c_set_double(const char *name, const double val)
{   
   PyObject *v;
   v=PyFloat_FromDouble(val);
   int ret;
   
   if(!v)
   {
      PRINTERROR;
      Py_XDECREF(v);
      return FAILURE;
   }
   ret=_setVar(name,v);
   Py_XDECREF(v);
   
   return ret;
}

int c_set_str(const char *name, const char *val)
{   
   PyObject *v;
   v=PyString_FromString(val);
   int ret;
   
   if(!v)
   {
      PRINTERROR;
      Py_XDECREF(v);
      return FAILURE;
   }
   
   ret=_setVar(name,v);
   Py_XDECREF(v);
   
   return ret;
}

int _print_dict(PyObject *dict)
{
   PyObject *key = NULL;
   PyObject *value = NULL;
   Py_ssize_t pos=0;
   
   while(PyDict_Next(dict,&pos,&key,&value))
   {
      printf("%s %s\n",PyString_AsString(key),PyString_AsString(PyObject_Str(value)));
   }
      
   Py_XDECREF(key);
   Py_XDECREF(value);
   return SUCCESS;
}

PyObject* _getVar(const char *name)
{
   PyObject *val = NULL;

   if(PyObject_HasAttrString(mainmod,name))
   {
      val=PyObject_GetAttrString(mainmod,name);
      return val;
   }   

   return val;
}

int _setVar(const char *name, PyObject *val)
{
   int ret;
   
   ret=PyObject_SetAttrString(mainmod,name,val);

   if(ret){
      PRINTERROR;
      return FAILURE;
   }
   
   return SUCCESS;
}
   


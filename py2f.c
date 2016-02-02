#include "py2f.h"
#include <stdio.h>

#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION

#include <numpy/arrayobject.h>
#include <numpy/npy_common.h>

#define SUCCESS 0
#define FAILURE -1

#if PY_MAJOR_VERSION >= 3
#define IS_PY3K
#endif

#define PRINTERROR printf ("Line %d of file %s (function %s)\n",\
                      __LINE__, __FILE__, __func__)

#define NOT_IMPLEMENTED printf("%s is not implemeneted yet\n",__func__)

PyObject *mainmod = NULL;
PyObject *main_dict = NULL;

int c_setup(void){
  Py_SetProgramName("python");
  Py_Initialize();

  mainmod=PyImport_ImportModule("__main__");
  main_dict=PyModule_GetDict(mainmod);

  import_array();

  return SUCCESS;
}

int c_finish(void){
   Py_Finalize();
   return SUCCESS;
}

int c_run(const char * restrict cmd){
   PyRun_SimpleString(cmd);
   return SUCCESS;
}

int c_dealloc(const char * restrict objname, const char * restrict name){
   return c_set_int(objname,name,0);
}


int c_get_str(const char * restrict objname, const char * restrict name, char ** restrict value){
   PyObject *obj = NULL;

   obj=_getVar(objname,name);

   if(!obj){
      PRINTERROR;
      printf("%s\n",name);
      return FAILURE;
   }

   *value=_PyString_AsString(obj);

   if(!value){
      PRINTERROR;
      printf("%s\n",name);
      return FAILURE;
   }

   Py_XDECREF(obj);

   return SUCCESS;
}

int c_get_int(const char * restrict objname, const char * restrict name, long int * restrict value){
   PyObject *obj = NULL;

   obj=_getVar(objname,name);

   if(!obj){
      PRINTERROR;
      printf("%s\n",name);
      return FAILURE;
   }

   *value=PyLong_AsLong(obj);

   if(!value){
      PRINTERROR;
      printf("%s\n",name);
      return FAILURE;
   }

   Py_XDECREF(obj);

   return SUCCESS;
}

int c_get_double(const char * restrict objname, const char * restrict name, double * restrict value){
   PyObject *obj = NULL;

   obj=_getVar(objname,name);
   if(!obj){
      PRINTERROR;
      printf("%s\n",name);
      return FAILURE;
   }

   *value=PyFloat_AsDouble(obj);

   if(!value){
      PRINTERROR;
      printf("%s\n",name);
      return FAILURE;
   }

   Py_XDECREF(obj);

   return SUCCESS;
}


int c_set_int(const char * restrict objname, const char * restrict name, const int val){
   PyObject *v;
   int ret;

   v=PyLong_FromLong(val);

   if(!v){
      PRINTERROR;
      Py_XDECREF(v);
      return FAILURE;
   }

   ret=_setVar(objname,name,v);
   Py_XDECREF(v);

   return ret;
}


int c_set_double(const char * restrict objname, const char * restrict name, const double val)
{
   PyObject *v;
   v=PyFloat_FromDouble(val);
   int ret;

   if(!v){
      PRINTERROR;
      Py_XDECREF(v);
      return FAILURE;
   }
   ret=_setVar(objname,name,v);
   Py_XDECREF(v);

   return ret;
}

int c_set_str(const char * restrict objname, const char * restrict name, const char * restrict val){
   PyObject *v;
   v=PyUnicode_FromString(val);
   int ret;

   if(!v){
      PRINTERROR;
      Py_XDECREF(v);
      return FAILURE;
   }

   ret=_setVar(objname,name,v);
   Py_XDECREF(v);

   return ret;
}

int c_set_double_array_multid(const char * restrict objname, const char * restrict name,
  const int ndims, int* restrict shape, double** restrict val){
   npy_intp *dims;
   PyObject *v=NULL;
   PyArrayObject *array=NULL;
   int ret,i;

   dims=malloc(ndims);
   if(!dims){
      Py_XDECREF(v);
      Py_XDECREF(array);
      return FAILURE;
   }


   for(i=0;i<ndims;i++){
      dims[i]=shape[i];
   }

   //Create empty array
   v=PyArray_SimpleNewFromData(ndims,dims,NPY_DOUBLE,val);

   array=(PyArrayObject*) v;
   PyArray_ENABLEFLAGS(array,NPY_ARRAY_F_CONTIGUOUS);
   PyArray_ENABLEFLAGS(array,NPY_ARRAY_OWNDATA);

   if(!v){
      PRINTERROR;
      Py_XDECREF(v);
      Py_XDECREF(array);
      return FAILURE;
   }

   ret=_setVar(objname,name,v);
   Py_XDECREF(v);
   Py_XDECREF(array);

   return ret;
}

int c_get_double_array_1d(const char * restrict objname, const char * restrict name, double** val){
   PyObject *obj = NULL;
   PyArrayObject *arr =NULL;
   PyArray_Descr *dtype = NULL;
   int ndim = 0;
   npy_intp *dims;

   dims=malloc(NPY_MAXDIMS);
   if(!dims){
      return FAILURE;
   }

   obj=_getVar(objname,name);
   if(!obj){
      PRINTERROR;
      printf("%s\n",name);
      return FAILURE;
   }


   if (PyArray_GetArrayParamsFromObject(obj, NULL, 0, &dtype,&ndim, dims, &arr, NULL) < 0 && !arr){
      Py_XDECREF(obj);
      PyArray_XDECREF(arr);
      return FAILURE;
   }

   *val=(double*) PyArray_DATA(arr);

   if(!val){
      PRINTERROR;
      printf("%s\n",name);
      Py_XDECREF(obj);
      PyArray_XDECREF(arr);
      return FAILURE;
   }

    Py_XDECREF(obj);
    PyArray_XDECREF(arr);
   return SUCCESS;
}


int c_set_int_array_multid(const char * restrict objname, const char * restrict name,
  const int ndims, int* restrict shape, int** restrict val){
   npy_intp *dims;
   PyObject *v=NULL;
   PyArrayObject *array=NULL;
   int ret,i;


   dims=malloc(ndims);
   if(!dims){
      Py_XDECREF(v);
      Py_XDECREF(array);
      return FAILURE;
   }
   for(i=0;i<ndims;i++){
      dims[i]=shape[i];
   }

   //Create empty array
   v=PyArray_SimpleNewFromData(ndims,dims,NPY_INT,val);
   array=(PyArrayObject*) v;
   PyArray_ENABLEFLAGS(array,NPY_ARRAY_F_CONTIGUOUS);
	 PyArray_ENABLEFLAGS(array,NPY_ARRAY_OWNDATA);

   if(!v){
      PRINTERROR;
      Py_XDECREF(v);
      Py_XDECREF(array);
      return FAILURE;
   }

   ret=_setVar(objname,name,v);
   Py_XDECREF(v);
   Py_XDECREF(array);

   return ret;
}



int c_get_int_array_1d(const char * restrict objname, const char * restrict name, int** val){
   PyObject *obj = NULL;
   PyArrayObject *arr =NULL;
   PyArray_Descr *dtype = NULL;
   int ndim = 0;
   npy_intp *dims;

   dims=malloc(NPY_MAXDIMS);
   if(!dims){
      return FAILURE;
   }

   obj=_getVar(objname,name);
   if(!obj){
      PRINTERROR;
      printf("%s\n",name);
      return FAILURE;
   }


   if (PyArray_GetArrayParamsFromObject(obj, NULL, 0, &dtype,&ndim, dims, &arr, NULL) < 0 && !arr){
      Py_XDECREF(obj);
      PyArray_XDECREF(arr);
      return FAILURE;
   }

   *val=(int*) PyArray_DATA(arr);

   if(!val){
      PRINTERROR;
      printf("%s\n",name);
      Py_XDECREF(obj);
      PyArray_XDECREF(arr);
      return FAILURE;
   }

    Py_XDECREF(obj);
    PyArray_XDECREF(arr);
    return SUCCESS;
}

//////////////////////////////////////
// Private functions
// You have been warned
//////////////////////////////////////


int _get_array_len(const char * restrict objname, const char * restrict name){
   return (int)PyArray_Size(_getVar(objname,name));
}



int _print_dict(PyObject *dict){
   PyObject *key = NULL;
   PyObject *value = NULL;
   Py_ssize_t pos=0;

   while(PyDict_Next(dict,&pos,&key,&value)){
      printf("%s %s\n",_PyString_AsString(key),_PyString_AsString(PyObject_Str(value)));
   }

   Py_XDECREF(key);
   Py_XDECREF(value);
   return SUCCESS;
}

PyObject* _getVar(const char * restrict objname, const char * restrict name)
{
   PyObject *val = NULL;
   PyObject *tmp = NULL;
   PyObject *pynull = NULL;


   if(PyObject_HasAttrString(mainmod,objname)){
      tmp=PyObject_GetAttrString(mainmod,objname);

      if(!tmp){
         PRINTERROR;
         printf("%s\n",name);
         Py_XDECREF(tmp);
         return pynull;
      }

      if(PyObject_HasAttrString(tmp,name)){
         val=PyObject_GetAttrString(tmp,name);
         Py_XDECREF(tmp);
      }
      else{
         printf("No variable %s in %s\n",name,objname);
         Py_XDECREF(tmp);
         return pynull;
      }
   }
   else if(PyObject_HasAttrString(mainmod,name)){
      val=PyObject_GetAttrString(mainmod,name);
   }

   return val;
}

int _setVar(const char * restrict objname, const char * restrict name, PyObject *val){
   int ret;
   PyObject *tmp = NULL;

   if(PyObject_HasAttrString(mainmod,objname)){
      tmp=PyObject_GetAttrString(mainmod,objname);

      if(!tmp){
         PRINTERROR;
         printf("%s\n",name);
         Py_XDECREF(tmp);
         return FAILURE;
      }

      ret=PyObject_SetAttrString(tmp,name,val);
      Py_XDECREF(tmp);
   }
   else{
      ret=PyObject_SetAttrString(mainmod,name,val);
   }

   if(ret){
      PRINTERROR;
      return FAILURE;
   }

   return SUCCESS;
}

void _print_object(PyObject *obj, const char* restrict  name){
   PyObject *repr = NULL;

   printf("DEBUG Object: %s\n",name);

   if(!obj){
      printf("Object is null\n");
      return;
   }

   repr=PyObject_Repr(obj);
   if(!repr){
      printf("Repr:\n");
      printf("%s\n",_PyString_AsString(repr));
   }
   repr=NULL;

   repr=PyObject_Type(obj);
   if(!repr){
      printf("Type:\n");
      printf("%s\n",_PyString_AsString(repr));
   }
   repr=NULL;

//    if(PyObject_HasAttrString(obj,"__doc__"))
//    {
//       printf("Doc:\n");
//       repr=PyObject_GetAttrString(obj,"__doc__");
//       if(!repr)
//       {
//          printf("Getting __doc__ failed\n");
//       }
//       else
//       {
//          printf("%s\n",_PyString_AsString(repr));
//       }
//
//    }
//    repr=NULL;

   if(PyObject_HasAttrString(obj,"__dict__")){
      printf("Dict:\n");
      repr=PyObject_GetAttrString(obj,"__dict__");
      _print_dict(repr);
      printf("End Dict\n");
   }
   repr=NULL;

   printf("END DEBUG %s\n",name);
   Py_XDECREF(repr);
   return;
}


char* _PyString_AsString(PyObject* str){

#if defined (IS_PY3K)
    PyObject * temp_bytes = PyUnicode_AsEncodedString(str, "ASCII", "strict"); // Owned reference
    char* res;
    if (temp_bytes != NULL) {
        res = strdup(PyBytes_AS_STRING(temp_bytes));
        Py_XDECREF(temp_bytes);
        return res;
    } else {
        Py_XDECREF(temp_bytes);
        return NULL;
    }
#else
   return PyString_AsString(str);
#endif
}

#include <Python.h>

int c_setup();
int c_finish();


int c_run(const char * restrict cmd);
// int c_load_module(const char *name);

int c_get_int(const char * restrict obj, const char * restrict name, long int * restrict value);
int c_get_double(const char * restrict obj, const char * restrict name, double * restrict value);
int c_get_str(const char * restrict obj, const char * restrict name, char ** restrict value);

int c_set_int(const char * restrict obj, const char * restrict name, const int val);
int c_set_double(const char * restrict obj, const char * restrict name, const double val);
int c_set_str(const char * restrict obj, const char * restrict name, const char* restrict  val);

int c_set_double_array_1d(const char * restrict objname, const char * restrict name, const int len, double* restrict  val);
int c_get_double_array_1d(const char * restrict objname, const char * restrict name, double** val);

int c_set_int_array_1d(const char * restrict objname, const char * restrict name, const int len, int* restrict  val);
int c_get_int_array_1d(const char * restrict objname, const char * restrict name, int** val);

//Private functions

int _get_array_size(const char * restrict obj, const char * restrict name);
int _print_dict(PyObject *dict);
PyObject* _getVar(const char * restrict obj, const char * restrict name);
int _setVar(const char * restrict obj, const char * restrict name, PyObject *val);
void _print_object(PyObject *obj, const char* restrict  name);


// Functions that need to handle Python2 and 3

char* _PyString_AsString(PyObject* str);

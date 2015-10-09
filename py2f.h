#include <Python.h>

int c_setup();
int c_finish();


int c_run(const char *cmd);
int c_load_module(const char *name);

int c_get_int(const char *obj, const char *name, long int *value);
int c_get_double(const char *obj, const char *name, double *value);
int c_get_str(const char *obj, const char *name, char **value);

int c_set_int(const char *obj, const char *name, const int val);
int c_set_double(const char *obj, const char *name, const double val);
int c_set_str(const char *obj, const char *name, const char* val);


//Private functions

int _print_dict(PyObject *dict);
PyObject* _getVar(const char *obj, const char *name);
int _setVar(const char *obj, const char *name, PyObject *val);
void _print_object(PyObject *obj, const char* name);
#include <Python.h>

int c_setup();
int c_finish();
int c_run(const char *cmd);
int c_load_module(const char *name);
int c_get_str(const char *module, const char *name, char **value);
int c_get_int(const char *module, const char *name, long int *value);
int c_set_int(const char *module, const char *name, const int val);

//Private functions

int _print_dict(PyObject *dict);
PyObject* _getVar(const char *module, const char *name);
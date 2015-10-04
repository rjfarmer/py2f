#include <Python.h>
#include "py2f.h"

int c_setup()
{
  Py_SetProgramName("name");  /* optional but recommended */
  Py_Initialize();
  return 0;
}

int c_finish()
{
   Py_Finalize();
   return 0;
}

int c_run(const char *cmd)
{
   PyRun_SimpleString(cmd);
   return 0;
}
   
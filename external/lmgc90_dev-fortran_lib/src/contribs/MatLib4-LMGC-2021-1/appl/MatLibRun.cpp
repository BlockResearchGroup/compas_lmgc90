/*
 *  $Id: MatLibRun.cpp 270 2020-04-24 17:06:25Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2020, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */

// Python library API
#include <Python.h>

// std C library
#include <cstdio>
#include <cstdlib>
#include <cstring>
// std C++ library
#include <iostream>


/*
 * Basic application to run Python scripts for MatLib.
 */
int main(int argc,char *argv[]) {
  
  // read input file name
  std::FILE *input;
  if (argc < 2)
    input = stdin;
  else {
    input = std::fopen(argv[1],"r");
    if (!input) {
      std::cerr << "ERROR: could not open file ";
      std::cerr << argv[1] << "." << std::endl;
      return -1;
    }
  }

  // pipe file to python interpreter
#if (PY_MAJOR_VERSION < 3)
  Py_SetProgramName(argv[0]);
#else
  size_t len = std::strlen(argv[0])+1;
  wchar_t long_name[len];
  std::mbstowcs(long_name,argv[0],len);
  Py_SetProgramName(long_name);
#endif
  Py_Initialize();
  PyRun_SimpleFile(input,argv[1]);
  Py_Finalize();
  
  return 0;
}

/*
 *  $Id: MatLibRun.cpp 218 2016-10-18 07:35:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */

// configuration parameters (to know if python3 is used)
#include "matlib_config.h"

// Python library API
#include <Python.h>

// std C library
#include <cstdio>
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
  #ifdef MATLIB_USE_PYTHON3
  wchar_t progname[FILENAME_MAX + 1];
  mbstowcs(progname, argv[0], strlen(argv[0]) + 1);
  Py_SetProgramName(progname);
  #else
  Py_SetProgramName(argv[0]);
  #endif
  Py_Initialize();
  PyRun_SimpleFile(input,argv[1]);
  Py_Finalize();
  
  return 0;
}

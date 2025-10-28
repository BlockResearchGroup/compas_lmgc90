/*
 *  $Id: MaterialCriterion.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "MaterialCriterion.h"

// std C library
#include <cstring>
// Visual Studio specific
#if defined(_WIN32) || defined(_WIN64)
#define strcasecmp _stricmp
#endif
// std C++ library
#include <fstream>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


// check consistency of material properties
void MaterialCriterion::checkProperties(MaterialProperties& material,
                                        const char* oFileName) 
 throw (FileException, InvalidPropertyException, NoSuchPropertyException) {
   
  // open output file
  if (oFileName) {
    if (strcasecmp(oFileName,"null")) {
      std::ofstream file(oFileName,std::ofstream::out | std::ofstream::app);
      if (!file.is_open()) {
        std::string msg("cannot open output file: ");
        msg += oFileName;
        throw FileException(msg);
      }
      checkProperties(material,&file);
    }
    else
      checkProperties(material,(std::ostream*)0);
  }
  else
    checkProperties(material,&std::cout);
}

/*
 *  $Id: Property.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "Property.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


// convert to string
std::string IntegerProperty::toString() const {
  O_STRING_STREAM os;
  os << val << " (integer)";
#ifdef HAVE_SSTREAM
  return os.str();
#else
  return std::string(os.str(),os.pcount());
#endif
}

// convert to string
std::string DoubleProperty::toString() const {
  O_STRING_STREAM os;
  os << val << " (real)";
#ifdef HAVE_SSTREAM
  return os.str();
#else
  return std::string(os.str(),os.pcount());
#endif
}

// convert to string
std::string FunctionProperty::toString() const {
  O_STRING_STREAM os;
  os << fct->getName() << " (function)";
#ifdef HAVE_SSTREAM
  return os.str();
#else
  return std::string(os.str(),os.pcount());
#endif
}

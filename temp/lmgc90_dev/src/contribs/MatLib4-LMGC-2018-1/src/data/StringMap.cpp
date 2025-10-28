/*
 *  $Id: StringMap.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "StringMap.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


// hash function for strings (taken from Stroustrup)
size_t HashString::operator()(const std::string& key) const {
  size_t res = 0;
  typedef std::string::const_iterator CI;
  CI p = key.begin();
  CI end = key.end();
  while(p!=end) res = (res<<1)^*p++;
  return res;
}

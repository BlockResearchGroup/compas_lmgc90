/*
 *  $Id: MaterialState.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "MaterialState.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class MaterialState
 */

// assignment operator
MaterialState& MaterialState::operator=(const MaterialState& src) {

  // copy standard data
  grad = src.grad;
  flux = src.flux;
  internal = src.internal;
  
  // copy extra data
  if (extra && src.extra) extra->copy(*(src.extra));
  
  return *this;
}



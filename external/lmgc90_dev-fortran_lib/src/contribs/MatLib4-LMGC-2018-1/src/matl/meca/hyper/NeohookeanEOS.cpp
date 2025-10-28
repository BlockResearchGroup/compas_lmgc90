/*
 *  $Id: NeohookeanEOS.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "NeohookeanEOS.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class NeohookeanEOSBuilder.
 */

// the instance
NeohookeanEOSBuilder const* NeohookeanEOSBuilder::BUILDER = new NeohookeanEOSBuilder();

// constructor
NeohookeanEOSBuilder::NeohookeanEOSBuilder() {
  ModelDictionary::add("NEOHOOKEAN_EOS",*this);
}

// build model
ConstitutiveModel* NeohookeanEOSBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NeohookeanEOS3D();
      break;
    case 2:
      return new NeohookeanEOS2D();
      break;
    case 1:
      return new NeohookeanEOS1D();
      break;
    default:
      return 0;
      break;
  }
}

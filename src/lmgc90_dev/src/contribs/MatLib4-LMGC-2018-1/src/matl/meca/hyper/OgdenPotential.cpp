/*
 *  $Id: OgdenPotential.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "OgdenPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class OgdenBuilder.
 */

// the instance
OgdenBuilder const* OgdenBuilder::BUILDER = new OgdenBuilder();

// constructor
OgdenBuilder::OgdenBuilder() {
  ModelDictionary::add("OGDEN",*this);
}

// build model
ConstitutiveModel* OgdenBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new Ogden3D();
      break;
    case 2:
      return new Ogden2D();
      break;
    case 1:
      return new Ogden1D();
      break;
    default:
      return 0;
      break;
  }
}

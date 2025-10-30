/*
 *  $Id: MooneyRivlinPotential.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "MooneyRivlinPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class MooneyRivlinBuilder.
 */

// the instance
MooneyRivlinBuilder const* MooneyRivlinBuilder::BUILDER = new MooneyRivlinBuilder();

// constructor
MooneyRivlinBuilder::MooneyRivlinBuilder() {
  ModelDictionary::add("MOONEY_RIVLIN",*this);
}

// build model
ConstitutiveModel* MooneyRivlinBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new MooneyRivlin3D();
      break;
    case 2:
      return new MooneyRivlin2D();
      break;
    case 1:
      return new MooneyRivlin1D();
      break;
    default:
      return 0;
      break;
  }
}

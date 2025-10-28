/*
 *  $Id: GentPotential.cpp 199 2016-03-10 20:35:29Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "GentPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class GentBuilder.
 */

// the instance
GentBuilder const* GentBuilder::BUILDER = new GentBuilder();

// constructor
GentBuilder::GentBuilder() {
  ModelDictionary::add("GENT",*this);
}

// build model
ConstitutiveModel* GentBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new Gent3D();
      break;
    case 2:
      return new Gent2D();
      break;
    case 1:
      return new Gent1D();
      break;
    default:
      return 0;
      break;
  }
}

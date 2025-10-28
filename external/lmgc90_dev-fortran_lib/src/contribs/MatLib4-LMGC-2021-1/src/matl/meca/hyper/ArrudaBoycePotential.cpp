/*
 *  $Id: ArrudaBoycePotential.cpp 199 2016-03-10 20:35:29Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "ArrudaBoycePotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class ArrudaBoyceBuilder.
 */

// the instance
ArrudaBoyceBuilder const* ArrudaBoyceBuilder::BUILDER = new ArrudaBoyceBuilder();

// constructor
ArrudaBoyceBuilder::ArrudaBoyceBuilder() {
  ModelDictionary::add("ARRUDA_BOYCE",*this);
}

// build model
ConstitutiveModel* ArrudaBoyceBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new ArrudaBoyce3D();
      break;
    case 2:
      return new ArrudaBoyce2D();
      break;
    case 1:
      return new ArrudaBoyce1D();
      break;
    default:
      return 0;
      break;
  }
}

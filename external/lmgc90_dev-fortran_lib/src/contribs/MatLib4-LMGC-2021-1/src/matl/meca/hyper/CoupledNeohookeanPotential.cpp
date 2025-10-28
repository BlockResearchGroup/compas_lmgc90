/*
 *  $Id: CoupledNeohookeanPotential.cpp 134 2013-07-22 19:05:07Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "CoupledNeohookeanPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class CoupledNeohookeanBuilder.
 */

// the instance
CoupledNeohookeanBuilder const* CoupledNeohookeanBuilder::BUILDER = new CoupledNeohookeanBuilder();

// constructor
CoupledNeohookeanBuilder::CoupledNeohookeanBuilder() {
  ModelDictionary::add("COUPLED_NEOHOOKEAN",*this);
}

// build model
ConstitutiveModel* CoupledNeohookeanBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledNeohookean3D();
      break;
    case 2:
      return new CoupledNeohookean2D();
      break;
    case 1:
      return new CoupledNeohookean1D();
      break;
    default:
      return 0;
      break;
  }
}

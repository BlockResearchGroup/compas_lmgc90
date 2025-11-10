/*
 *  $Id: CubicElasticPotential.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "CubicElasticPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class CubicElasticityBuilder.
 */

// the instance
CubicElasticityBuilder const* CubicElasticityBuilder::BUILDER 
  = new CubicElasticityBuilder();

// constructor
CubicElasticityBuilder::CubicElasticityBuilder() {
  ModelDictionary::add("CUBIC_ELASTICITY",*this);
}

// build model
ConstitutiveModel* CubicElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CubicElasticity3D();
      break;
    case 2:
      return new CubicElasticity2D();
      break;
    case 1:
      return new CubicElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

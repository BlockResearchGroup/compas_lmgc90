/*
 *  $Id: IsotropicElasticPotential.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicElasticPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicElasticityBuilder.
 */

// the instance
IsotropicElasticityBuilder const* IsotropicElasticityBuilder::BUILDER 
  = new IsotropicElasticityBuilder();

// constructor
IsotropicElasticityBuilder::IsotropicElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicElasticity3D();
      break;
    case 2:
      return new IsotropicElasticity2D();
      break;
    case 1:
      return new IsotropicElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

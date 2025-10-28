/*
 *  $Id: IsotropicChemoElasticity.cpp 169 2015-08-10 09:34:30Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicChemoElasticity.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicChemoElasticityBuilder.
 */

// the instance
IsotropicChemoElasticityBuilder const* IsotropicChemoElasticityBuilder::BUILDER
= new IsotropicChemoElasticityBuilder();

// constructor
IsotropicChemoElasticityBuilder::IsotropicChemoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_CHEMO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicChemoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicChemoElasticity3D();
      break;
    case 2:
      return new IsotropicChemoElasticity2D();
      break;
    case 1:
      return new IsotropicChemoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

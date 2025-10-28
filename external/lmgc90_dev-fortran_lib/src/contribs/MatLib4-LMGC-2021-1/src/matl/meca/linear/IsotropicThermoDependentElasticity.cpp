/*
 *  $Id: IsotropicThermoDependentElasticity.cpp 204 2016-06-29 13:42:20Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicThermoDependentElasticity.h"


#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicThermoDependentElasticityBuilder.
 */

// the instance
IsotropicThermoDependentElasticityBuilder const* IsotropicThermoDependentElasticityBuilder::BUILDER
  = new IsotropicThermoDependentElasticityBuilder();

// constructor
IsotropicThermoDependentElasticityBuilder::IsotropicThermoDependentElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_THERMO_DEPENDENT_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicThermoDependentElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicThermoDependentElasticity3D();
      break;
    case 2:
      return new IsotropicThermoDependentElasticity2D();
      break;
    case 1:
      return new IsotropicThermoDependentElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


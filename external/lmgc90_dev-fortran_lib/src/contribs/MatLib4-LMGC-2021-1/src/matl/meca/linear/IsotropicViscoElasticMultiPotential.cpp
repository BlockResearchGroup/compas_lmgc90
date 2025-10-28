/*
 *  $Id: IsotropicViscoElasticMultiPotential.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicViscoElasticMultiPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicMaxwellViscoElasticityBuilder.
 */

// the instance
IsotropicMaxwellViscoElasticityBuilder const* IsotropicMaxwellViscoElasticityBuilder::BUILDER 
= new IsotropicMaxwellViscoElasticityBuilder();

// constructor
IsotropicMaxwellViscoElasticityBuilder::IsotropicMaxwellViscoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_MAXWELL_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicMaxwellViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicMaxwellViscoElasticity3D();
      break;
    case 2:
      return new IsotropicMaxwellViscoElasticity2D();
      break;
    case 1:
      return new IsotropicMaxwellViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class IsotropicViscoElasticityBuilder.
 */

// the instance
IsotropicViscoElasticityBuilder const* IsotropicViscoElasticityBuilder::BUILDER 
= new IsotropicViscoElasticityBuilder();

// constructor
IsotropicViscoElasticityBuilder::IsotropicViscoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicViscoElasticity3D();
      break;
    case 2:
      return new IsotropicViscoElasticity2D();
      break;
    case 1:
      return new IsotropicViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

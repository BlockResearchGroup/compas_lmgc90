/*
 *  $Id: IsotropicHenckyViscoHEMultiPotential.cpp 252 2018-05-18 11:58:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2018, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicHenckyViscoHEMultiPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicHenckyMaxwellViscoElasticityBuilder.
 */

// the instance
IsotropicHenckyMaxwellViscoElasticityBuilder const* IsotropicHenckyMaxwellViscoElasticityBuilder::BUILDER 
= new IsotropicHenckyMaxwellViscoElasticityBuilder();

// constructor
IsotropicHenckyMaxwellViscoElasticityBuilder::IsotropicHenckyMaxwellViscoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_HENCKY_MAXWELL_VISCO_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicHenckyMaxwellViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicHenckyMaxwellViscoElasticity3D(0,true);
      break;
    case 2:
      return new IsotropicHenckyMaxwellViscoElasticity2D(0,true);
      break;
    case 1:
      return new IsotropicHenckyMaxwellViscoElasticity1D(0,true);
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class IsotropicHenckyViscoElasticityBuilder.
 */

// the instance
IsotropicHenckyViscoElasticityBuilder const* IsotropicHenckyViscoElasticityBuilder::BUILDER
= new IsotropicHenckyViscoElasticityBuilder();

// constructor
IsotropicHenckyViscoElasticityBuilder::IsotropicHenckyViscoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_HENCKY_VISCO_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicHenckyViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicHenckyViscoElasticity3D(0,true);
      break;
    case 2:
      return new IsotropicHenckyViscoElasticity2D(0,true);
      break;
    case 1:
      return new IsotropicHenckyViscoElasticity1D(0,true);
      break;
    default:
      return 0;
      break;
  }
}


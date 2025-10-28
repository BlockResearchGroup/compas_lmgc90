/*
 *  $Id: IsotropicHenckyViscousPotential.cpp 252 2018-05-18 11:58:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2018, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicHenckyViscousPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicHenckyViscoHyperElasticityBuilder.
 */

// the instance
IsotropicHenckyKelvinViscoElasticityBuilder const* IsotropicHenckyKelvinViscoElasticityBuilder::BUILDER
= new IsotropicHenckyKelvinViscoElasticityBuilder();

// constructor
IsotropicHenckyKelvinViscoElasticityBuilder::IsotropicHenckyKelvinViscoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_HENCKY_KELVIN_VISCO_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicHenckyKelvinViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicHenckyKelvinViscoElasticity3D();
      break;
    case 2:
      return new IsotropicHenckyKelvinViscoElasticity2D();
      break;
    case 1:
      return new IsotropicHenckyKelvinViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}



/*
 *  $Id: ThermalHenckyViscousPotential.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2020, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicThermalHenckyViscousPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicThermalHenckyViscoHyperElasticityBuilder.
 */

// the instance
IsotropicThermalHenckyViscoHyperElasticityBuilder const* IsotropicThermalHenckyViscoHyperElasticityBuilder::BUILDER
= new IsotropicThermalHenckyViscoHyperElasticityBuilder();

// constructor
IsotropicThermalHenckyViscoHyperElasticityBuilder::IsotropicThermalHenckyViscoHyperElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_THERMAL_HENCKY_VISCO_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicThermalHenckyViscoHyperElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicThermalHenckyViscoHyperElasticity3D();
      break;
    case 2:
      return new IsotropicThermalHenckyViscoHyperElasticity2D();
      break;
    case 1:
      return new IsotropicThermalHenckyViscoHyperElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


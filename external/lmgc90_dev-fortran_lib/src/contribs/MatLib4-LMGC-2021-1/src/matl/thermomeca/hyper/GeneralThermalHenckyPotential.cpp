/*
 *  $Id: GeneralThermalHenckyPotential.cpp 127 2013-03-07 02:15:51Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "GeneralThermalHenckyPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicThermoHyperElasticityBuilder.
 */

// the instance
IsotropicThermoHyperElasticityBuilder const* IsotropicThermoHyperElasticityBuilder::BUILDER 
  = new IsotropicThermoHyperElasticityBuilder();

// constructor
IsotropicThermoHyperElasticityBuilder::IsotropicThermoHyperElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_THERMO_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicThermoHyperElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicThermoHyperElasticity3D();
      break;
    case 2:
      return new IsotropicThermoHyperElasticity2D();
      break;
    case 1:
      return new IsotropicThermoHyperElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

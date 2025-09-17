/*
 *  $Id: IsotropicThermoViscoElasticMultiPotential.cpp 215 2016-10-06 20:46:04Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicThermoViscoElasticMultiPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicMaxwellThermoViscoElasticityBuilder.
 */

// the instance
IsotropicMaxwellThermoViscoElasticityBuilder const* IsotropicMaxwellThermoViscoElasticityBuilder::BUILDER
= new IsotropicMaxwellThermoViscoElasticityBuilder();

// constructor
IsotropicMaxwellThermoViscoElasticityBuilder::IsotropicMaxwellThermoViscoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_MAXWELL_THERMO_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicMaxwellThermoViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicMaxwellThermoViscoElasticity3D();
      break;
    case 2:
      return new IsotropicMaxwellThermoViscoElasticity2D();
      break;
    case 1:
      return new IsotropicMaxwellThermoViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class IsotropicThermoViscoElasticityBuilder.
 */

// the instance
IsotropicThermoViscoElasticityBuilder const* IsotropicThermoViscoElasticityBuilder::BUILDER
= new IsotropicThermoViscoElasticityBuilder();

// constructor
IsotropicThermoViscoElasticityBuilder::IsotropicThermoViscoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_THERMO_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicThermoViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicThermoViscoElasticity3D();
      break;
    case 2:
      return new IsotropicThermoViscoElasticity2D();
      break;
    case 1:
      return new IsotropicThermoViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 *  $Id: IsotropicThermalViscousPotential.cpp 127 2013-03-07 02:15:51Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicThermalViscousPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicKelvinThermoViscoElasticityBuilder.
 */

// the instance
IsotropicKelvinThermoViscoElasticityBuilder const* IsotropicKelvinThermoViscoElasticityBuilder::BUILDER 
  = new IsotropicKelvinThermoViscoElasticityBuilder();

// constructor
IsotropicKelvinThermoViscoElasticityBuilder::IsotropicKelvinThermoViscoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_KELVIN_THERMO_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicKelvinThermoViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicKelvinThermoViscoElasticity3D();
      break;
    case 2:
      return new IsotropicKelvinThermoViscoElasticity2D();
      break;
    case 1:
      return new IsotropicKelvinThermoViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

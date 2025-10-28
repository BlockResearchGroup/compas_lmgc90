/*
 *  $Id: IsotropicThermoElasticity.cpp 127 2013-03-07 02:15:51Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicThermoElasticity.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicThermoElasticityBuilder.
 */

// the instance
IsotropicThermoElasticityBuilder const* IsotropicThermoElasticityBuilder::BUILDER 
= new IsotropicThermoElasticityBuilder();

// constructor
IsotropicThermoElasticityBuilder::IsotropicThermoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_THERMO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicThermoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicThermoElasticity3D();
      break;
    case 2:
      return new IsotropicThermoElasticity2D();
      break;
    case 1:
      return new IsotropicThermoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

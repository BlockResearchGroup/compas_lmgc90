/*
 *  $Id: LinSimpleThMInterfaceModels.cpp 127 2013-03-07 02:15:51Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "LinSimpleThMInterfaceModels.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicThermoElasticInterfaceBuilder.
 */

// the instance
IsotropicThermoElasticInterfaceBuilder const* IsotropicThermoElasticInterfaceBuilder::BUILDER 
= new IsotropicThermoElasticInterfaceBuilder();

// constructor
IsotropicThermoElasticInterfaceBuilder::IsotropicThermoElasticInterfaceBuilder() {
  ModelDictionary::add("ISOTROPIC_THERMO_ELASTIC_INTERFACE",*this);
}

// build model
ConstitutiveModel* IsotropicThermoElasticInterfaceBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicThermoElasticInterface3D();
      break;
    case 2:
      return new IsotropicThermoElasticInterface2D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class LinearIsotropicThermoPlasticInterfaceBuilder.
 */

// the instance
LinearIsotropicThermoPlasticInterfaceBuilder const* LinearIsotropicThermoPlasticInterfaceBuilder::BUILDER 
= new LinearIsotropicThermoPlasticInterfaceBuilder();

// constructor
LinearIsotropicThermoPlasticInterfaceBuilder::LinearIsotropicThermoPlasticInterfaceBuilder() {
  ModelDictionary::add("LINEAR_ISOTROPIC_THERMO_PLASTIC_INTERFACE",*this);
}

// build model
ConstitutiveModel* LinearIsotropicThermoPlasticInterfaceBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearIsotropicThermoPlasticInterface3D();
      break;
    case 2:
      return new LinearIsotropicThermoPlasticInterface2D();
      break;
    default:
      return 0;
      break;
  }
}


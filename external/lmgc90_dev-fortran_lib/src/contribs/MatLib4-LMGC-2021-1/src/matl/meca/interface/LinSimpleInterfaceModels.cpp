/*
 *  $Id: LinSimpleInterfaceModels.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "LinSimpleInterfaceModels.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicElasticInterfaceBuilder.
 */

// the instance
IsotropicElasticInterfaceBuilder const* IsotropicElasticInterfaceBuilder::BUILDER 
= new IsotropicElasticInterfaceBuilder();

// constructor
IsotropicElasticInterfaceBuilder::IsotropicElasticInterfaceBuilder() {
  ModelDictionary::add("ISOTROPIC_ELASTIC_INTERFACE",*this);
}

// build model
ConstitutiveModel* IsotropicElasticInterfaceBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicElasticInterface3D();
      break;
    case 2:
      return new IsotropicElasticInterface2D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class LinearIsotropicPlasticInterfaceBuilder.
 */

// the instance
LinearIsotropicPlasticInterfaceBuilder const* LinearIsotropicPlasticInterfaceBuilder::BUILDER 
= new LinearIsotropicPlasticInterfaceBuilder();

// constructor
LinearIsotropicPlasticInterfaceBuilder::LinearIsotropicPlasticInterfaceBuilder() {
  ModelDictionary::add("LINEAR_ISOTROPIC_PLASTIC_INTERFACE",*this);
}

// build model
ConstitutiveModel* LinearIsotropicPlasticInterfaceBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearIsotropicPlasticInterface3D();
      break;
    case 2:
      return new LinearIsotropicPlasticInterface2D();
      break;
    default:
      return 0;
      break;
  }
}


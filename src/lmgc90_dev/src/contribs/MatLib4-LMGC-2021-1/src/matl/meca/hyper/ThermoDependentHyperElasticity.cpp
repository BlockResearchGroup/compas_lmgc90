/*
 *  $Id$
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2021, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "ThermoDependentHyperElasticity.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicThermoDependentHyperElasticityBuilder.
 */

// the instance
IsotropicThermoDependentHyperElasticityBuilder const* IsotropicThermoDependentHyperElasticityBuilder::BUILDER 
  = new IsotropicThermoDependentHyperElasticityBuilder();

// constructor
IsotropicThermoDependentHyperElasticityBuilder::IsotropicThermoDependentHyperElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_THERMO_DEPENDENT_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicThermoDependentHyperElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicThermoDependentHyperElasticity3D();
      break;
    case 2:
      return new IsotropicThermoDependentHyperElasticity2D();
      break;
    case 1:
      return new IsotropicThermoDependentHyperElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

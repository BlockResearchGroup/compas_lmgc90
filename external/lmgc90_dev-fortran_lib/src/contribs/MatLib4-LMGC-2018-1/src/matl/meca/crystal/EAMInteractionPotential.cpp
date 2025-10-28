/*
 *  $Id: EAMInteractionPotential.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "EAMInteractionPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class EAMHyperElasticityBuilder.
 */

// the instance
EAMHyperElasticityBuilder const* EAMHyperElasticityBuilder::BUILDER 
= new EAMHyperElasticityBuilder();

// constructor
EAMHyperElasticityBuilder::EAMHyperElasticityBuilder() {
  ModelDictionary::add("EAM_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* EAMHyperElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new EAMHyperElasticity3D();
      break;
    case 2:
      return new EAMHyperElasticity2D();
      break;
    case 1:
      return new EAMHyperElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


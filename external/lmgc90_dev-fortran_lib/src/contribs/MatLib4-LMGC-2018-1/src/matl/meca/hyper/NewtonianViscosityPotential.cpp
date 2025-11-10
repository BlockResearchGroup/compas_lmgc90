/*
 *  $Id: NewtonianViscosityPotential.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "NewtonianViscosityPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class NewtonianViscoHyperElasticityBuilder.
 */

// the instance
NewtonianViscoHyperElasticityBuilder const* NewtonianViscoHyperElasticityBuilder::BUILDER 
= new NewtonianViscoHyperElasticityBuilder();

// constructor
NewtonianViscoHyperElasticityBuilder::NewtonianViscoHyperElasticityBuilder() {
  ModelDictionary::add("NEWTONIAN_VISCO_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* NewtonianViscoHyperElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NewtonianViscoHyperElasticity3D();
      break;
    case 2:
      return new NewtonianViscoHyperElasticity2D();
      break;
    case 1:
      return new NewtonianViscoHyperElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


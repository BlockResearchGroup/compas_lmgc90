/*
 *  $Id: GeneralHenckyPotential.cpp 237 2017-06-06 09:13:56Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "GeneralHenckyPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicHyperElasticityBuilder.
 */

// the instance
IsotropicHyperElasticityBuilder const* IsotropicHyperElasticityBuilder::BUILDER 
  = new IsotropicHyperElasticityBuilder();

// constructor
IsotropicHyperElasticityBuilder::IsotropicHyperElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicHyperElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicHyperElasticity3D();
      break;
    case 2:
      return new IsotropicHyperElasticity2D();
      break;
    case 1:
      return new IsotropicHyperElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class StVenantKirchhoffHyperElasticityBuilder.
 */

// the instance
StVenantKirchhoffHyperElasticityBuilder const* StVenantKirchhoffHyperElasticityBuilder::BUILDER
= new StVenantKirchhoffHyperElasticityBuilder();

// constructor
StVenantKirchhoffHyperElasticityBuilder::StVenantKirchhoffHyperElasticityBuilder() {
  ModelDictionary::add("ST_VENANT_KIRCHHOFF_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* StVenantKirchhoffHyperElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new StVenantKirchhoffHyperElasticity3D();
      break;
    case 2:
      return new StVenantKirchhoffHyperElasticity2D();
      break;
    case 1:
      return new StVenantKirchhoffHyperElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CubicHyperElasticityBuilder.
 */

// the instance
CubicHyperElasticityBuilder const* CubicHyperElasticityBuilder::BUILDER 
= new CubicHyperElasticityBuilder();

// constructor
CubicHyperElasticityBuilder::CubicHyperElasticityBuilder() {
  ModelDictionary::add("CUBIC_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* CubicHyperElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CubicHyperElasticity3D();
      break;
    case 2:
      return new CubicHyperElasticity2D();
      break;
    case 1:
      return new CubicHyperElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class OrthotropicHyperElasticityBuilder.
 */

// the instance
OrthotropicHyperElasticityBuilder const* OrthotropicHyperElasticityBuilder::BUILDER
= new OrthotropicHyperElasticityBuilder();

// constructor
OrthotropicHyperElasticityBuilder::OrthotropicHyperElasticityBuilder() {
  ModelDictionary::add("ORTHOTROPIC_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* OrthotropicHyperElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new OrthotropicHyperElasticity3D();
      break;
    case 2:
      return new OrthotropicHyperElasticity2D();
      break;
    case 1:
      return new OrthotropicHyperElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

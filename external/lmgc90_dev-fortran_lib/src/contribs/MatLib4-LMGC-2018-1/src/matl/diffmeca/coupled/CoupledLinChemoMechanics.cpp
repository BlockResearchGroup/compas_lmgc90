/*
 *  $Id: CoupledLinChemoMechanics.cpp 239 2017-06-09 15:01:17Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "CoupledLinChemoMechanics.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class CoupledIsotropicChemoElasticityBuilder.
 */

// the instance
CoupledIsotropicChemoElasticityBuilder const* CoupledIsotropicChemoElasticityBuilder::BUILDER
= new CoupledIsotropicChemoElasticityBuilder();

// constructor
CoupledIsotropicChemoElasticityBuilder::CoupledIsotropicChemoElasticityBuilder() {
  ModelDictionary::add("COUPLED_ISOTROPIC_CHEMO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledIsotropicChemoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledIsotropicChemoElasticity3D();
      break;
    case 2:
      return new CoupledIsotropicChemoElasticity2D();
      break;
    case 1:
      return new CoupledIsotropicChemoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledLinearIsotropicJ2WkChemoPlasticityBuilder.
 */

// the instance
CoupledLinearIsotropicJ2WkChemoPlasticityBuilder const* CoupledLinearIsotropicJ2WkChemoPlasticityBuilder::BUILDER
= new CoupledLinearIsotropicJ2WkChemoPlasticityBuilder();

// constructor
CoupledLinearIsotropicJ2WkChemoPlasticityBuilder::CoupledLinearIsotropicJ2WkChemoPlasticityBuilder() {
  ModelDictionary::add("COUPLED_LINEAR_ISOTROPIC_J2_CHEMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledLinearIsotropicJ2WkChemoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledLinearIsotropicJ2WkChemoPlasticity3D();
      break;
    case 2:
      return new CoupledLinearIsotropicJ2WkChemoPlasticity2D();
      break;
    case 1:
      return new CoupledLinearIsotropicJ2WkChemoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledLinearIsotropicJ2WkChemoViscoPlasticityBuilder.
 */

// the instance
CoupledLinearIsotropicJ2WkChemoViscoPlasticityBuilder const* CoupledLinearIsotropicJ2WkChemoViscoPlasticityBuilder::BUILDER
= new CoupledLinearIsotropicJ2WkChemoViscoPlasticityBuilder();

// constructor
CoupledLinearIsotropicJ2WkChemoViscoPlasticityBuilder::CoupledLinearIsotropicJ2WkChemoViscoPlasticityBuilder() {
  ModelDictionary::add("COUPLED_LINEAR_ISOTROPIC_J2_CHEMO_VISCO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledLinearIsotropicJ2WkChemoViscoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledLinearIsotropicJ2WkChemoViscoPlasticity3D();
      break;
    case 2:
      return new CoupledLinearIsotropicJ2WkChemoViscoPlasticity2D();
      break;
    case 1:
      return new CoupledLinearIsotropicJ2WkChemoViscoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledLinearIsotropicJ2ChemoPlasticityBuilder.
 */

// the instance
CoupledLinearIsotropicJ2ChemoPlasticityBuilder const* CoupledLinearIsotropicJ2ChemoPlasticityBuilder::BUILDER
= new CoupledLinearIsotropicJ2ChemoPlasticityBuilder();

// constructor
CoupledLinearIsotropicJ2ChemoPlasticityBuilder::CoupledLinearIsotropicJ2ChemoPlasticityBuilder() {
  ModelDictionary::add("COUPLED_LINEAR_ISOTROPIC_J2_CHEMO_PLASTICITY_ELLIPTIC",*this);
}

// build model
ConstitutiveModel* CoupledLinearIsotropicJ2ChemoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledLinearIsotropicJ2ChemoPlasticity3D();
      break;
    case 2:
      return new CoupledLinearIsotropicJ2ChemoPlasticity2D();
      break;
    case 1:
      return new CoupledLinearIsotropicJ2ChemoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledLinearIsotropicJ2ChemoViscoPlasticityBuilder.
 */

// the instance
CoupledLinearIsotropicJ2ChemoViscoPlasticityBuilder const* CoupledLinearIsotropicJ2ChemoViscoPlasticityBuilder::BUILDER
= new CoupledLinearIsotropicJ2ChemoViscoPlasticityBuilder();

// constructor
CoupledLinearIsotropicJ2ChemoViscoPlasticityBuilder::CoupledLinearIsotropicJ2ChemoViscoPlasticityBuilder() {
  ModelDictionary::add("COUPLED_LINEAR_ISOTROPIC_J2_CHEMO_VISCO_PLASTICITY_ELLIPTIC",*this);
}

// build model
ConstitutiveModel* CoupledLinearIsotropicJ2ChemoViscoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledLinearIsotropicJ2ChemoViscoPlasticity3D();
      break;
    case 2:
      return new CoupledLinearIsotropicJ2ChemoViscoPlasticity2D();
      break;
    case 1:
      return new CoupledLinearIsotropicJ2ChemoViscoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}



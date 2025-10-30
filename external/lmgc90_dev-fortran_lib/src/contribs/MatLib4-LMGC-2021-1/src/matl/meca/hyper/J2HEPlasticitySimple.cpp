/*
 *  $Id: J2HEPlasticitySimple.cpp 141 2014-01-27 20:57:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "J2HEPlasticitySimple.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class LinearIsotropicJ2HEPlasticityBuilder.
 */

// the instance
LinearIsotropicJ2HEPlasticityBuilder const* LinearIsotropicJ2HEPlasticityBuilder::BUILDER 
= new LinearIsotropicJ2HEPlasticityBuilder();

// constructor
LinearIsotropicJ2HEPlasticityBuilder::LinearIsotropicJ2HEPlasticityBuilder() {
  ModelDictionary::add("LINEAR_ISOTROPIC_J2_FINITE_PLASTICITY",*this);
}

// build model
ConstitutiveModel* LinearIsotropicJ2HEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearIsotropicJ2HEPlasticity3D();
      break;
    case 2:
      return new LinearIsotropicJ2HEPlasticity2D();
      break;
    case 1:
      return new LinearIsotropicJ2HEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class NonLinearIsotropicJ2HEPlasticityBuilder.
 */

// the instance
NonLinearIsotropicJ2HEPlasticityBuilder const* NonLinearIsotropicJ2HEPlasticityBuilder::BUILDER 
= new NonLinearIsotropicJ2HEPlasticityBuilder();

// constructor
NonLinearIsotropicJ2HEPlasticityBuilder::NonLinearIsotropicJ2HEPlasticityBuilder() {
  ModelDictionary::add("NONLINEAR_ISOTROPIC_J2_FINITE_PLASTICITY",*this);
}

// build model
ConstitutiveModel* NonLinearIsotropicJ2HEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NonLinearIsotropicJ2HEPlasticity3D();
      break;
    case 2:
      return new NonLinearIsotropicJ2HEPlasticity2D();
      break;
    case 1:
      return new NonLinearIsotropicJ2HEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class NonLinearASinhIsotropicJ2HEPlasticityBuilder.
 */

// the instance
NonLinearASinhIsotropicJ2HEPlasticityBuilder const* NonLinearASinhIsotropicJ2HEPlasticityBuilder::BUILDER 
= new NonLinearASinhIsotropicJ2HEPlasticityBuilder();

// constructor
NonLinearASinhIsotropicJ2HEPlasticityBuilder::NonLinearASinhIsotropicJ2HEPlasticityBuilder() {
  ModelDictionary::add("NONLINEAR_ASINH_ISOTROPIC_J2_FINITE_PLASTICITY",*this);
}

// build model
ConstitutiveModel* NonLinearASinhIsotropicJ2HEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NonLinearASinhIsotropicJ2HEPlasticity3D();
      break;
    case 2:
      return new NonLinearASinhIsotropicJ2HEPlasticity2D();
      break;
    case 1:
      return new NonLinearASinhIsotropicJ2HEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class NortonHoffIsotropicJ2HEPlasticityBuilder.
 */

// the instance
NortonHoffIsotropicJ2HEPlasticityBuilder const* NortonHoffIsotropicJ2HEPlasticityBuilder::BUILDER 
= new NortonHoffIsotropicJ2HEPlasticityBuilder();

// constructor
NortonHoffIsotropicJ2HEPlasticityBuilder::NortonHoffIsotropicJ2HEPlasticityBuilder() {
  ModelDictionary::add("NORTON_HOFF_ISOTROPIC_J2_FINITE_PLASTICITY",*this);
}

// build model
ConstitutiveModel* NortonHoffIsotropicJ2HEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NortonHoffIsotropicJ2HEPlasticity3D();
      break;
    case 2:
      return new NortonHoffIsotropicJ2HEPlasticity2D();
      break;
    case 1:
      return new NortonHoffIsotropicJ2HEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


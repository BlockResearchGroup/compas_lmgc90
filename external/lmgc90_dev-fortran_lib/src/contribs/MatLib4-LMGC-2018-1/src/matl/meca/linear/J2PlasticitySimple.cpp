/*
 *  $Id: J2PlasticitySimple.cpp 153 2014-10-03 09:12:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "J2PlasticitySimple.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class LinearIsotropicJ2PlasticityBuilder.
 */

// the instance
LinearIsotropicJ2PlasticityBuilder const* LinearIsotropicJ2PlasticityBuilder::BUILDER 
= new LinearIsotropicJ2PlasticityBuilder();

// constructor
LinearIsotropicJ2PlasticityBuilder::LinearIsotropicJ2PlasticityBuilder() {
  ModelDictionary::add("LINEAR_ISOTROPIC_J2_PLASTICITY",*this);
}

// build model
ConstitutiveModel* LinearIsotropicJ2PlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearIsotropicJ2Plasticity3D();
      break;
    case 2:
      return new LinearIsotropicJ2Plasticity2D();
      break;
    case 1:
      return new LinearIsotropicJ2Plasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class NonLinearIsotropicJ2PlasticityBuilder.
 */

// the instance
NonLinearIsotropicJ2PlasticityBuilder const* NonLinearIsotropicJ2PlasticityBuilder::BUILDER 
= new NonLinearIsotropicJ2PlasticityBuilder();

// constructor
NonLinearIsotropicJ2PlasticityBuilder::NonLinearIsotropicJ2PlasticityBuilder() {
  ModelDictionary::add("NONLINEAR_ISOTROPIC_J2_PLASTICITY",*this);
}

// build model
ConstitutiveModel* NonLinearIsotropicJ2PlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NonLinearIsotropicJ2Plasticity3D();
      break;
    case 2:
      return new NonLinearIsotropicJ2Plasticity2D();
      break;
    case 1:
      return new NonLinearIsotropicJ2Plasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class NonLinearASinhIsotropicJ2PlasticityBuilder.
 */

// the instance
NonLinearASinhIsotropicJ2PlasticityBuilder const* NonLinearASinhIsotropicJ2PlasticityBuilder::BUILDER
= new NonLinearASinhIsotropicJ2PlasticityBuilder();

// constructor
NonLinearASinhIsotropicJ2PlasticityBuilder::NonLinearASinhIsotropicJ2PlasticityBuilder() {
  ModelDictionary::add("NONLINEAR_ASINH_ISOTROPIC_J2_PLASTICITY",*this);
}

// build model
ConstitutiveModel* NonLinearASinhIsotropicJ2PlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NonLinearASinhIsotropicJ2Plasticity3D();
      break;
    case 2:
      return new NonLinearASinhIsotropicJ2Plasticity2D();
      break;
    case 1:
      return new NonLinearASinhIsotropicJ2Plasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class NortonHoffIsotropicJ2PlasticityBuilder.
 */

// the instance
NortonHoffIsotropicJ2PlasticityBuilder const* NortonHoffIsotropicJ2PlasticityBuilder::BUILDER 
= new NortonHoffIsotropicJ2PlasticityBuilder();

// constructor
NortonHoffIsotropicJ2PlasticityBuilder::NortonHoffIsotropicJ2PlasticityBuilder() {
  ModelDictionary::add("NORTON_HOFF_ISOTROPIC_J2_PLASTICITY",*this);
}

// build model
ConstitutiveModel* NortonHoffIsotropicJ2PlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NortonHoffIsotropicJ2Plasticity3D();
      break;
    case 2:
      return new NortonHoffIsotropicJ2Plasticity2D();
      break;
    case 1:
      return new NortonHoffIsotropicJ2Plasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 *  $Id: J2ThermoPlasticitySimple.cpp 153 2014-10-03 09:12:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "J2ThermoPlasticitySimple.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class LinearIsotropicJ2ThermoPlasticityBuilder.
 */

// the instance
LinearIsotropicJ2ThermoPlasticityBuilder const* LinearIsotropicJ2ThermoPlasticityBuilder::BUILDER 
= new LinearIsotropicJ2ThermoPlasticityBuilder();

// constructor
LinearIsotropicJ2ThermoPlasticityBuilder::LinearIsotropicJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("LINEAR_ISOTROPIC_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* LinearIsotropicJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearIsotropicJ2ThermoPlasticity3D();
      break;
    case 2:
      return new LinearIsotropicJ2ThermoPlasticity2D();
      break;
    case 1:
      return new LinearIsotropicJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class NonLinearIsotropicJ2ThermoPlasticityBuilder.
 */

// the instance
NonLinearIsotropicJ2ThermoPlasticityBuilder const* NonLinearIsotropicJ2ThermoPlasticityBuilder::BUILDER 
= new NonLinearIsotropicJ2ThermoPlasticityBuilder();

// constructor
NonLinearIsotropicJ2ThermoPlasticityBuilder::NonLinearIsotropicJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("NONLINEAR_ISOTROPIC_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* NonLinearIsotropicJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NonLinearIsotropicJ2ThermoPlasticity3D();
      break;
    case 2:
      return new NonLinearIsotropicJ2ThermoPlasticity2D();
      break;
    case 1:
      return new NonLinearIsotropicJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class NonLinearASinhIsotropicJ2ThermoPlasticityBuilder.
 */

// the instance
NonLinearASinhIsotropicJ2ThermoPlasticityBuilder const* NonLinearASinhIsotropicJ2ThermoPlasticityBuilder::BUILDER 
= new NonLinearASinhIsotropicJ2ThermoPlasticityBuilder();

// constructor
NonLinearASinhIsotropicJ2ThermoPlasticityBuilder::NonLinearASinhIsotropicJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("NONLINEAR_ASINH_ISOTROPIC_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* NonLinearASinhIsotropicJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NonLinearASinhIsotropicJ2ThermoPlasticity3D();
      break;
    case 2:
      return new NonLinearASinhIsotropicJ2ThermoPlasticity2D();
      break;
    case 1:
      return new NonLinearASinhIsotropicJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class NortonHoffIsotropicJ2ThermoPlasticityBuilder.
 */

// the instance
NortonHoffIsotropicJ2ThermoPlasticityBuilder const* NortonHoffIsotropicJ2ThermoPlasticityBuilder::BUILDER
= new NortonHoffIsotropicJ2ThermoPlasticityBuilder();

// constructor
NortonHoffIsotropicJ2ThermoPlasticityBuilder::NortonHoffIsotropicJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("NORTON_HOFF_ISOTROPIC_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* NortonHoffIsotropicJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NortonHoffIsotropicJ2ThermoPlasticity3D();
      break;
    case 2:
      return new NortonHoffIsotropicJ2ThermoPlasticity2D();
      break;
    case 1:
      return new NortonHoffIsotropicJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

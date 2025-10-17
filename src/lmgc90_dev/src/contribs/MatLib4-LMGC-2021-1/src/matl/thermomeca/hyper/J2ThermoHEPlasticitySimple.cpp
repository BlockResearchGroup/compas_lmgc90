/*
 *  $Id: J2ThermoHEPlasticitySimple.cpp 153 2014-10-03 09:12:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "J2ThermoHEPlasticitySimple.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class LinearIsotropicJ2ThermoHEPlasticityBuilder.
 */

// the instance
LinearIsotropicJ2ThermoHEPlasticityBuilder const* LinearIsotropicJ2ThermoHEPlasticityBuilder::BUILDER 
= new LinearIsotropicJ2ThermoHEPlasticityBuilder();

// constructor
LinearIsotropicJ2ThermoHEPlasticityBuilder::LinearIsotropicJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("LINEAR_ISOTROPIC_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* LinearIsotropicJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearIsotropicJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new LinearIsotropicJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new LinearIsotropicJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class NonLinearIsotropicJ2ThermoHEPlasticityBuilder.
 */

// the instance
NonLinearIsotropicJ2ThermoHEPlasticityBuilder const* NonLinearIsotropicJ2ThermoHEPlasticityBuilder::BUILDER 
= new NonLinearIsotropicJ2ThermoHEPlasticityBuilder();

// constructor
NonLinearIsotropicJ2ThermoHEPlasticityBuilder::NonLinearIsotropicJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("NONLINEAR_ISOTROPIC_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* NonLinearIsotropicJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NonLinearIsotropicJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new NonLinearIsotropicJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new NonLinearIsotropicJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class NonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder.
 */

// the instance
NonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder const* NonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder::BUILDER 
= new NonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder();

// constructor
NonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder::NonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("NONLINEAR_ASINH_ISOTROPIC_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* NonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NonLinearASinhIsotropicJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new NonLinearASinhIsotropicJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new NonLinearASinhIsotropicJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class NortonHoffIsotropicJ2ThermoHEPlasticityBuilder.
 */

// the instance
NortonHoffIsotropicJ2ThermoHEPlasticityBuilder const* NortonHoffIsotropicJ2ThermoHEPlasticityBuilder::BUILDER
= new NortonHoffIsotropicJ2ThermoHEPlasticityBuilder();

// constructor
NortonHoffIsotropicJ2ThermoHEPlasticityBuilder::NortonHoffIsotropicJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("NORTON_HOFF_ISOTROPIC_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* NortonHoffIsotropicJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NortonHoffIsotropicJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new NortonHoffIsotropicJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new NortonHoffIsotropicJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

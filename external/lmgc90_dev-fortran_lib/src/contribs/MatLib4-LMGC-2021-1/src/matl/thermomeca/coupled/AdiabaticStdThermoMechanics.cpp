/*
 *  $Id: AdiabaticStdThermoMechanics.cpp 153 2014-10-03 09:12:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "AdiabaticStdThermoMechanics.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class AdiabaticIsotropicThermoHyperElasticityBuilder.
 */

// the instance
AdiabaticIsotropicThermoHyperElasticityBuilder const* AdiabaticIsotropicThermoHyperElasticityBuilder::BUILDER 
= new AdiabaticIsotropicThermoHyperElasticityBuilder();

// constructor
AdiabaticIsotropicThermoHyperElasticityBuilder::AdiabaticIsotropicThermoHyperElasticityBuilder() {
  ModelDictionary::add("ADIABATIC_ISOTROPIC_THERMO_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* AdiabaticIsotropicThermoHyperElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new AdiabaticIsotropicThermoHyperElasticity3D();
      break;
    case 2:
      return new AdiabaticIsotropicThermoHyperElasticity2D();
      break;
    case 1:
      return new AdiabaticIsotropicThermoHyperElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class AdiabaticLinearIsotropicJ2ThermoHEPlasticityBuilder.
 */

// the instance
AdiabaticLinearIsotropicJ2ThermoHEPlasticityBuilder const* AdiabaticLinearIsotropicJ2ThermoHEPlasticityBuilder::BUILDER 
= new AdiabaticLinearIsotropicJ2ThermoHEPlasticityBuilder();

// constructor
AdiabaticLinearIsotropicJ2ThermoHEPlasticityBuilder::AdiabaticLinearIsotropicJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("ADIABATIC_LINEAR_ISOTROPIC_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* AdiabaticLinearIsotropicJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new AdiabaticLinearIsotropicJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new AdiabaticLinearIsotropicJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new AdiabaticLinearIsotropicJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class AdiabaticNonLinearIsotropicJ2ThermoHEPlasticityBuilder.
 */

// the instance
AdiabaticNonLinearIsotropicJ2ThermoHEPlasticityBuilder const* AdiabaticNonLinearIsotropicJ2ThermoHEPlasticityBuilder::BUILDER 
= new AdiabaticNonLinearIsotropicJ2ThermoHEPlasticityBuilder();

// constructor
AdiabaticNonLinearIsotropicJ2ThermoHEPlasticityBuilder::AdiabaticNonLinearIsotropicJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("ADIABATIC_NONLINEAR_ISOTROPIC_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* AdiabaticNonLinearIsotropicJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder.
 */

// the instance
AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder const* AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder::BUILDER 
= new AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder();

// constructor
AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder::AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("ADIABATIC_NONLINEAR_ASINH_ISOTROPIC_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticityBuilder.
 */

// the instance
AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticityBuilder const* AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticityBuilder::BUILDER
= new AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticityBuilder();

// constructor
AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticityBuilder::AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("ADIABATIC_NORTON_HOFF_ISOTROPIC_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


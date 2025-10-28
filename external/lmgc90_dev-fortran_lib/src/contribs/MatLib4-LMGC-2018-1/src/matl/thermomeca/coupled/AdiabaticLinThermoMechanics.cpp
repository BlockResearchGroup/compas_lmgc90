/*
 *  $Id: AdiabaticLinThermoMechanics.cpp 153 2014-10-03 09:12:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "AdiabaticLinThermoMechanics.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class AdiabaticIsotropicThermoElasticityBuilder.
 */

// the instance
AdiabaticIsotropicThermoElasticityBuilder const* AdiabaticIsotropicThermoElasticityBuilder::BUILDER 
= new AdiabaticIsotropicThermoElasticityBuilder();

// constructor
AdiabaticIsotropicThermoElasticityBuilder::AdiabaticIsotropicThermoElasticityBuilder() {
  ModelDictionary::add("ADIABATIC_ISOTROPIC_THERMO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* AdiabaticIsotropicThermoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new AdiabaticIsotropicThermoElasticity3D();
      break;
    case 2:
      return new AdiabaticIsotropicThermoElasticity2D();
      break;
    case 1:
      return new AdiabaticIsotropicThermoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class AdiabaticLinearIsotropicJ2ThermoPlasticityBuilder.
 */

// the instance
AdiabaticLinearIsotropicJ2ThermoPlasticityBuilder const* AdiabaticLinearIsotropicJ2ThermoPlasticityBuilder::BUILDER 
= new AdiabaticLinearIsotropicJ2ThermoPlasticityBuilder();

// constructor
AdiabaticLinearIsotropicJ2ThermoPlasticityBuilder::AdiabaticLinearIsotropicJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("ADIABATIC_LINEAR_ISOTROPIC_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* AdiabaticLinearIsotropicJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new AdiabaticLinearIsotropicJ2ThermoPlasticity3D();
      break;
    case 2:
      return new AdiabaticLinearIsotropicJ2ThermoPlasticity2D();
      break;
    case 1:
      return new AdiabaticLinearIsotropicJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class AdiabaticNonLinearIsotropicJ2ThermoPlasticityBuilder.
 */

// the instance
AdiabaticNonLinearIsotropicJ2ThermoPlasticityBuilder const* AdiabaticNonLinearIsotropicJ2ThermoPlasticityBuilder::BUILDER 
= new AdiabaticNonLinearIsotropicJ2ThermoPlasticityBuilder();

// constructor
AdiabaticNonLinearIsotropicJ2ThermoPlasticityBuilder::AdiabaticNonLinearIsotropicJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("ADIABATIC_NONLINEAR_ISOTROPIC_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* AdiabaticNonLinearIsotropicJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new AdiabaticNonLinearIsotropicJ2ThermoPlasticity3D();
      break;
    case 2:
      return new AdiabaticNonLinearIsotropicJ2ThermoPlasticity2D();
      break;
    case 1:
      return new AdiabaticNonLinearIsotropicJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticityBuilder.
 */

// the instance
AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticityBuilder const* AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticityBuilder::BUILDER 
= new AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticityBuilder();

// constructor
AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticityBuilder::AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("ADIABATIC_NONLINEAR_ASINH_ISOTROPIC_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity3D();
      break;
    case 2:
      return new AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity2D();
      break;
    case 1:
      return new AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class AdiabaticNortonHoffIsotropicJ2ThermoPlasticityBuilder.
 */

// the instance
AdiabaticNortonHoffIsotropicJ2ThermoPlasticityBuilder const* AdiabaticNortonHoffIsotropicJ2ThermoPlasticityBuilder::BUILDER
= new AdiabaticNortonHoffIsotropicJ2ThermoPlasticityBuilder();

// constructor
AdiabaticNortonHoffIsotropicJ2ThermoPlasticityBuilder::AdiabaticNortonHoffIsotropicJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("ADIABATIC_NORTON_HOFF_ISOTROPIC_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* AdiabaticNortonHoffIsotropicJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new AdiabaticNortonHoffIsotropicJ2ThermoPlasticity3D();
      break;
    case 2:
      return new AdiabaticNortonHoffIsotropicJ2ThermoPlasticity2D();
      break;
    case 1:
      return new AdiabaticNortonHoffIsotropicJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class AdiabaticIsotropicKelvinThermoViscoElasticityBuilder.
 */

// the instance
AdiabaticIsotropicKelvinThermoViscoElasticityBuilder const* AdiabaticIsotropicKelvinThermoViscoElasticityBuilder::BUILDER 
= new AdiabaticIsotropicKelvinThermoViscoElasticityBuilder();

// constructor
AdiabaticIsotropicKelvinThermoViscoElasticityBuilder::AdiabaticIsotropicKelvinThermoViscoElasticityBuilder() {
  ModelDictionary::add("ADIABATIC_ISOTROPIC_KELVIN_THERMO_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* AdiabaticIsotropicKelvinThermoViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new AdiabaticIsotropicKelvinThermoViscoElasticity3D();
      break;
    case 2:
      return new AdiabaticIsotropicKelvinThermoViscoElasticity2D();
      break;
    case 1:
      return new AdiabaticIsotropicKelvinThermoViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


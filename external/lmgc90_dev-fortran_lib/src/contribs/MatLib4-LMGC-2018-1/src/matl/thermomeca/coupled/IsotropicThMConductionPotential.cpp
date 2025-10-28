/*
 *  $Id: IsotropicThMConductionPotential.cpp 156 2014-10-15 20:26:02Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicThMConductionPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class CoupledIsotropicThermoHyperElasticityBuilder.
 */

// the instance
CoupledIsotropicThermoHyperElasticityBuilder const* CoupledIsotropicThermoHyperElasticityBuilder::BUILDER 
= new CoupledIsotropicThermoHyperElasticityBuilder();

// constructor
CoupledIsotropicThermoHyperElasticityBuilder::CoupledIsotropicThermoHyperElasticityBuilder() {
  ModelDictionary::add("COUPLED_ISOTROPIC_THERMO_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledIsotropicThermoHyperElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledIsotropicThermoHyperElasticity3D();
      break;
    case 2:
      return new CoupledIsotropicThermoHyperElasticity2D();
      break;
    case 1:
      return new CoupledIsotropicThermoHyperElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledLinearIsotropicJ2ThermoHEPlasticityBuilder.
 */

// the instance
CoupledLinearIsotropicJ2ThermoHEPlasticityBuilder const* CoupledLinearIsotropicJ2ThermoHEPlasticityBuilder::BUILDER 
= new CoupledLinearIsotropicJ2ThermoHEPlasticityBuilder();

// constructor
CoupledLinearIsotropicJ2ThermoHEPlasticityBuilder::CoupledLinearIsotropicJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("COUPLED_LINEAR_ISOTROPIC_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledLinearIsotropicJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledLinearIsotropicJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new CoupledLinearIsotropicJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new CoupledLinearIsotropicJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledNonLinearIsotropicJ2ThermoHEPlasticityBuilder.
 */

// the instance
CoupledNonLinearIsotropicJ2ThermoHEPlasticityBuilder const* CoupledNonLinearIsotropicJ2ThermoHEPlasticityBuilder::BUILDER 
= new CoupledNonLinearIsotropicJ2ThermoHEPlasticityBuilder();

// constructor
CoupledNonLinearIsotropicJ2ThermoHEPlasticityBuilder::CoupledNonLinearIsotropicJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("COUPLED_NONLINEAR_ISOTROPIC_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledNonLinearIsotropicJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledNonLinearIsotropicJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new CoupledNonLinearIsotropicJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new CoupledNonLinearIsotropicJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder.
 */

// the instance
CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder const* CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder::BUILDER 
= new CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder();

// constructor
CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder::CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("COUPLED_NONLINEAR_ASINH_ISOTROPIC_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledNortonHoffIsotropicJ2ThermoHEPlasticityBuilder.
 */

// the instance
CoupledNortonHoffIsotropicJ2ThermoHEPlasticityBuilder const* CoupledNortonHoffIsotropicJ2ThermoHEPlasticityBuilder::BUILDER
= new CoupledNortonHoffIsotropicJ2ThermoHEPlasticityBuilder();

// constructor
CoupledNortonHoffIsotropicJ2ThermoHEPlasticityBuilder::CoupledNortonHoffIsotropicJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("COUPLED_NORTON_HOFF_ISOTROPIC_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledNortonHoffIsotropicJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledNortonHoffIsotropicJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new CoupledNortonHoffIsotropicJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new CoupledNortonHoffIsotropicJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


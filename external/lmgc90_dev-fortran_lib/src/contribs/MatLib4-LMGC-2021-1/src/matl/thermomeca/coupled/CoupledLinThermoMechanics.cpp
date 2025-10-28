/*
 *  $Id: CoupledLinThermoMechanics.cpp 156 2014-10-15 20:26:02Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "CoupledLinThermoMechanics.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class CoupledIsotropicThermoElasticityBuilder.
 */

// the instance
CoupledIsotropicThermoElasticityBuilder const* CoupledIsotropicThermoElasticityBuilder::BUILDER 
= new CoupledIsotropicThermoElasticityBuilder();

// constructor
CoupledIsotropicThermoElasticityBuilder::CoupledIsotropicThermoElasticityBuilder() {
  ModelDictionary::add("COUPLED_ISOTROPIC_THERMO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledIsotropicThermoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledIsotropicThermoElasticity3D();
      break;
    case 2:
      return new CoupledIsotropicThermoElasticity2D();
      break;
    case 1:
      return new CoupledIsotropicThermoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledLinearIsotropicJ2ThermoPlasticityBuilder.
 */

// the instance
CoupledLinearIsotropicJ2ThermoPlasticityBuilder const* CoupledLinearIsotropicJ2ThermoPlasticityBuilder::BUILDER 
= new CoupledLinearIsotropicJ2ThermoPlasticityBuilder();

// constructor
CoupledLinearIsotropicJ2ThermoPlasticityBuilder::CoupledLinearIsotropicJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("COUPLED_LINEAR_ISOTROPIC_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledLinearIsotropicJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledLinearIsotropicJ2ThermoPlasticity3D();
      break;
    case 2:
      return new CoupledLinearIsotropicJ2ThermoPlasticity2D();
      break;
    case 1:
      return new CoupledLinearIsotropicJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledNonLinearIsotropicJ2ThermoPlasticityBuilder.
 */

// the instance
CoupledNonLinearIsotropicJ2ThermoPlasticityBuilder const* CoupledNonLinearIsotropicJ2ThermoPlasticityBuilder::BUILDER 
= new CoupledNonLinearIsotropicJ2ThermoPlasticityBuilder();

// constructor
CoupledNonLinearIsotropicJ2ThermoPlasticityBuilder::CoupledNonLinearIsotropicJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("COUPLED_NONLINEAR_ISOTROPIC_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledNonLinearIsotropicJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledNonLinearIsotropicJ2ThermoPlasticity3D();
      break;
    case 2:
      return new CoupledNonLinearIsotropicJ2ThermoPlasticity2D();
      break;
    case 1:
      return new CoupledNonLinearIsotropicJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledNonLinearASinhIsotropicJ2ThermoPlasticityBuilder.
 */

// the instance
CoupledNonLinearASinhIsotropicJ2ThermoPlasticityBuilder const* CoupledNonLinearASinhIsotropicJ2ThermoPlasticityBuilder::BUILDER
= new CoupledNonLinearASinhIsotropicJ2ThermoPlasticityBuilder();

// constructor
CoupledNonLinearASinhIsotropicJ2ThermoPlasticityBuilder::CoupledNonLinearASinhIsotropicJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("COUPLED_NONLINEAR_ASINH_ISOTROPIC_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledNonLinearASinhIsotropicJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledNonLinearASinhIsotropicJ2ThermoPlasticity3D();
      break;
    case 2:
      return new CoupledNonLinearASinhIsotropicJ2ThermoPlasticity2D();
      break;
    case 1:
      return new CoupledNonLinearASinhIsotropicJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledNortonHoffIsotropicJ2ThermoPlasticityBuilder.
 */

// the instance
CoupledNortonHoffIsotropicJ2ThermoPlasticityBuilder const* CoupledNortonHoffIsotropicJ2ThermoPlasticityBuilder::BUILDER
= new CoupledNortonHoffIsotropicJ2ThermoPlasticityBuilder();

// constructor
CoupledNortonHoffIsotropicJ2ThermoPlasticityBuilder::CoupledNortonHoffIsotropicJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("COUPLED_NORTON_HOFF_ISOTROPIC_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledNortonHoffIsotropicJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledNortonHoffIsotropicJ2ThermoPlasticity3D();
      break;
    case 2:
      return new CoupledNortonHoffIsotropicJ2ThermoPlasticity2D();
      break;
    case 1:
      return new CoupledNortonHoffIsotropicJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledIsotropicKelvinThermoViscoElasticityBuilder.
 */

// the instance
CoupledIsotropicKelvinThermoViscoElasticityBuilder const* CoupledIsotropicKelvinThermoViscoElasticityBuilder::BUILDER 
= new CoupledIsotropicKelvinThermoViscoElasticityBuilder();

// constructor
CoupledIsotropicKelvinThermoViscoElasticityBuilder::CoupledIsotropicKelvinThermoViscoElasticityBuilder() {
  ModelDictionary::add("COUPLED_ISOTROPIC_KELVIN_THERMO_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledIsotropicKelvinThermoViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledIsotropicKelvinThermoViscoElasticity3D();
      break;
    case 2:
      return new CoupledIsotropicKelvinThermoViscoElasticity2D();
      break;
    case 1:
      return new CoupledIsotropicKelvinThermoViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledThermoElasticSMAModel1Builder.
 */

// the instance
CoupledThermoElasticSMAModel1Builder const* CoupledThermoElasticSMAModel1Builder::BUILDER 
= new CoupledThermoElasticSMAModel1Builder();

// constructor
CoupledThermoElasticSMAModel1Builder::CoupledThermoElasticSMAModel1Builder() {
  ModelDictionary::add("COUPLED_ISOTROPIC_THERMO_ELASTIC_SMA_1",*this);
}

// build model
ConstitutiveModel* CoupledThermoElasticSMAModel1Builder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledThermoElasticSMAModel1_3D();
      break;
    case 2:
      return new CoupledThermoElasticSMAModel1_2D();
      break;
    case 1:
      return new CoupledThermoElasticSMAModel1_1D();
      break;
    default:
      return 0;
      break;
  }
}


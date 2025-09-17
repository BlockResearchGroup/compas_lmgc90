/*
 *  $Id: IsotropicViscousPotential.cpp 252 2018-05-18 11:58:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2018, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicViscousPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicKelvinViscoElasticityBuilder.
 */

// the instance
IsotropicKelvinViscoElasticityBuilder const* IsotropicKelvinViscoElasticityBuilder::BUILDER 
  = new IsotropicKelvinViscoElasticityBuilder();

// constructor
IsotropicKelvinViscoElasticityBuilder::IsotropicKelvinViscoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_KELVIN_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicKelvinViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicKelvinViscoElasticity3D();
      break;
    case 2:
      return new IsotropicKelvinViscoElasticity2D();
      break;
    case 1:
      return new IsotropicKelvinViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class IsotropicThermoDilatantKelvinViscoElasticityBuilder.
 */

// the instance
IsotropicThermoDilatantKelvinViscoElasticityBuilder const* IsotropicThermoDilatantKelvinViscoElasticityBuilder::BUILDER
= new IsotropicThermoDilatantKelvinViscoElasticityBuilder();

// constructor
IsotropicThermoDilatantKelvinViscoElasticityBuilder::IsotropicThermoDilatantKelvinViscoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_THERMO_DILATANT_KELVIN_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicThermoDilatantKelvinViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicThermoDilatantKelvinViscoElasticity3D();
      break;
    case 2:
      return new IsotropicThermoDilatantKelvinViscoElasticity2D();
      break;
    case 1:
      return new IsotropicThermoDilatantKelvinViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

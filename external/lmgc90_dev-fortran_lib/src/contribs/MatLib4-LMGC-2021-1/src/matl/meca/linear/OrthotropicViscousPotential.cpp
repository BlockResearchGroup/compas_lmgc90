/*
 *  $Id$
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2020, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "OrthotropicViscousPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class OrthotropicKelvinViscoElasticityBuilder.
 */

// the instance
OrthotropicKelvinViscoElasticityBuilder const* OrthotropicKelvinViscoElasticityBuilder::BUILDER
  = new OrthotropicKelvinViscoElasticityBuilder();

// constructor
OrthotropicKelvinViscoElasticityBuilder::OrthotropicKelvinViscoElasticityBuilder() {
  ModelDictionary::add("ORTHOTROPIC_KELVIN_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* OrthotropicKelvinViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new OrthotropicKelvinViscoElasticity3D();
      break;
    case 2:
      return new OrthotropicKelvinViscoElasticity2D();
      break;
    case 1:
      return new OrthotropicKelvinViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class OrthotropicThermoDilatantKelvinViscoElasticityBuilder.
 */

// the instance
OrthotropicThermoDilatantKelvinViscoElasticityBuilder const* OrthotropicThermoDilatantKelvinViscoElasticityBuilder::BUILDER
  = new OrthotropicThermoDilatantKelvinViscoElasticityBuilder();

// constructor
OrthotropicThermoDilatantKelvinViscoElasticityBuilder::OrthotropicThermoDilatantKelvinViscoElasticityBuilder() {
  ModelDictionary::add("ORTHOTROPIC_THERMO_DILATANT_KELVIN_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* OrthotropicThermoDilatantKelvinViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new OrthotropicThermoDilatantKelvinViscoElasticity3D();
      break;
    case 2:
      return new OrthotropicThermoDilatantKelvinViscoElasticity2D();
      break;
    case 1:
      return new OrthotropicThermoDilatantKelvinViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

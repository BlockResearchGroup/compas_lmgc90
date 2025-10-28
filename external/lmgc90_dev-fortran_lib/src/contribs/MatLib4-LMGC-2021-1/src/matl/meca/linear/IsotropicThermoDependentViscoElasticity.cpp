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
#include "IsotropicThermoDependentViscoElasticity.h"


#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicThermoDependentKelvinViscoElasticityBuilder.
 */

// the instance
IsotropicThermoDependentKelvinViscoElasticityBuilder const* IsotropicThermoDependentKelvinViscoElasticityBuilder::BUILDER
  = new IsotropicThermoDependentKelvinViscoElasticityBuilder();

// constructor
IsotropicThermoDependentKelvinViscoElasticityBuilder::IsotropicThermoDependentKelvinViscoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_THERMO_DEPENDENT_KELVIN_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicThermoDependentKelvinViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicThermoDependentKelvinViscoElasticity3D();
      break;
    case 2:
      return new IsotropicThermoDependentKelvinViscoElasticity2D();
      break;
    case 1:
      return new IsotropicThermoDependentKelvinViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class IsotropicMaxwellThermoDependentViscoElasticityBuilder.
 */

// the instance
IsotropicMaxwellThermoDependentViscoElasticityBuilder const* IsotropicMaxwellThermoDependentViscoElasticityBuilder::BUILDER
= new IsotropicMaxwellThermoDependentViscoElasticityBuilder();

// constructor
IsotropicMaxwellThermoDependentViscoElasticityBuilder::IsotropicMaxwellThermoDependentViscoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_MAXWELL_THERMO_DEPENDENT_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicMaxwellThermoDependentViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicMaxwellThermoDependentViscoElasticity3D();
      break;
    case 2:
      return new IsotropicMaxwellThermoDependentViscoElasticity2D();
      break;
    case 1:
      return new IsotropicMaxwellThermoDependentViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class IsotropicThermoDependentViscoElasticityBuilder.
 */

// the instance
IsotropicThermoDependentViscoElasticityBuilder const* IsotropicThermoDependentViscoElasticityBuilder::BUILDER
= new IsotropicThermoDependentViscoElasticityBuilder();

// constructor
IsotropicThermoDependentViscoElasticityBuilder::IsotropicThermoDependentViscoElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_THERMO_DEPENDENT_VISCO_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicThermoDependentViscoElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicThermoDependentViscoElasticity3D();
      break;
    case 2:
      return new IsotropicThermoDependentViscoElasticity2D();
      break;
    case 1:
      return new IsotropicThermoDependentViscoElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

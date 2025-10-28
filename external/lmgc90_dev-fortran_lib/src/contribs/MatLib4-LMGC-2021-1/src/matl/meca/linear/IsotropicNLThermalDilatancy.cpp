/*
 *  $Id: IsotropicNLThermalDilatancy.cpp 204 2016-06-29 13:42:20Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicNLThermalDilatancy.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicNLThermoDilatantElasticityBuilder.
 */

// the instance
IsotropicNLThermoDilatantElasticityBuilder const* IsotropicNLThermoDilatantElasticityBuilder::BUILDER
  = new IsotropicNLThermoDilatantElasticityBuilder();

// constructor
IsotropicNLThermoDilatantElasticityBuilder::IsotropicNLThermoDilatantElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_NONLINEAR_THERMO_DILATANT_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicNLThermoDilatantElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicNLThermoDilatantElasticity3D();
      break;
    case 2:
      return new IsotropicNLThermoDilatantElasticity2D();
      break;
    case 1:
      return new IsotropicNLThermoDilatantElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


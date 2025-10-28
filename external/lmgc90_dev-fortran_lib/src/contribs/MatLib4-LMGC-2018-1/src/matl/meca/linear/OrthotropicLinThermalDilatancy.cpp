/*
 *  $Id: OrthotropicLinThermalDilatancy.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "OrthotropicLinThermalDilatancy.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class OrthotropicThermoDilatantElasticityBuilder.
 */

// the instance
OrthotropicThermoDilatantElasticityBuilder const* OrthotropicThermoDilatantElasticityBuilder::BUILDER 
  = new OrthotropicThermoDilatantElasticityBuilder();

// constructor
OrthotropicThermoDilatantElasticityBuilder::OrthotropicThermoDilatantElasticityBuilder() {
  ModelDictionary::add("ORTHOTROPIC_THERMO_DILATANT_ELASTICITY",*this);
}

// build model
ConstitutiveModel* OrthotropicThermoDilatantElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new OrthotropicThermoDilatantElasticity3D();
      break;
    case 2:
      return new OrthotropicThermoDilatantElasticity2D();
      break;
    case 1:
      return new OrthotropicThermoDilatantElasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


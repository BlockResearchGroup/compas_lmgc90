/*
 *  $Id: IsotropicStdThermalDilatancy.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicStdThermalDilatancy.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicThermoDilatantHyperElasticityBuilder.
 */

// the instance
IsotropicThermoDilatantHyperElasticityBuilder const* IsotropicThermoDilatantHyperElasticityBuilder::BUILDER 
  = new IsotropicThermoDilatantHyperElasticityBuilder();

// constructor
IsotropicThermoDilatantHyperElasticityBuilder::IsotropicThermoDilatantHyperElasticityBuilder() {
  ModelDictionary::add("ISOTROPIC_THERMO_DILATANT_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* IsotropicThermoDilatantHyperElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicThermoDilatantHyperElasticity3D();
      break;
    case 2:
      return new IsotropicThermoDilatantHyperElasticity2D();
      break;
    /*case 1:
      return new IsotropicThermoDilatantHyperElasticity1D();
      break;*/
    default:
      return 0;
      break;
  }
}


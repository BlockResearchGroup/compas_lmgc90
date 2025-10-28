/*
 *  $Id: J2PlasticityMixed.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "J2PlasticityMixed.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class LinearMixedJ2PlasticityBuilder.
 */

// the instance
LinearMixedJ2PlasticityBuilder const* LinearMixedJ2PlasticityBuilder::BUILDER 
= new LinearMixedJ2PlasticityBuilder();

// constructor
LinearMixedJ2PlasticityBuilder::LinearMixedJ2PlasticityBuilder() {
  ModelDictionary::add("LINEAR_MIXED_J2_PLASTICITY",*this);
}

// build model
ConstitutiveModel* LinearMixedJ2PlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearMixedJ2Plasticity3D();
      break;
    case 2:
      return new LinearMixedJ2Plasticity2D();
      break;
    case 1:
      return new LinearMixedJ2Plasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

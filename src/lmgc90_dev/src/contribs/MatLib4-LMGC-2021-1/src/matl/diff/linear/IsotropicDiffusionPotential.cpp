/*
 *  $Id: IsotropicDiffusionPotential.cpp 162 2015-03-24 09:00:40Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicDiffusionPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicLinearDiffusionBuilder.
 */

// the instance
IsotropicLinearDiffusionBuilder const* IsotropicLinearDiffusionBuilder::BUILDER
= new IsotropicLinearDiffusionBuilder();

// constructor
IsotropicLinearDiffusionBuilder::IsotropicLinearDiffusionBuilder() {
  ModelDictionary::add("ISOTROPIC_LINEAR_DIFFUSION",*this);
}

// build model
ConstitutiveModel* IsotropicLinearDiffusionBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicLinearDiffusion3D();
      break;
    case 2:
      return new IsotropicLinearDiffusion2D();
      break;
    case 1:
      return new IsotropicLinearDiffusion1D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 *  $Id: IsotropicLinDiffusionPotential.cpp 167 2015-05-14 17:01:14Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicLinDiffusionPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicLinVariationalDiffusionBuilder.
 */

// the instance
IsotropicLinVariationalDiffusionBuilder const* IsotropicLinVariationalDiffusionBuilder::BUILDER
= new IsotropicLinVariationalDiffusionBuilder();

// constructor
IsotropicLinVariationalDiffusionBuilder::IsotropicLinVariationalDiffusionBuilder() {
  ModelDictionary::add("ISOTROPIC_LINEAR_VARIATIONAL_DIFFUSION",*this);
}

// build model
ConstitutiveModel* IsotropicLinVariationalDiffusionBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicLinVariationalDiffusion3D();
      break;
    case 2:
      return new IsotropicLinVariationalDiffusion2D();
      break;
    case 1:
      return new IsotropicLinVariationalDiffusion1D();
      break;
    default:
      return 0;
      break;
  }
}


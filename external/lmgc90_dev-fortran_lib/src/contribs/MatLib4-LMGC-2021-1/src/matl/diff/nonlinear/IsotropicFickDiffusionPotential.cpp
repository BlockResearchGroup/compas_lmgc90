/*
 *  $Id: IsotropicFickDiffusionPotential.cpp 207 2016-08-19 16:52:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicFickDiffusionPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicFickVariationalDiffusionBuilder.
 */

// the instance
IsotropicFickVariationalDiffusionBuilder const* IsotropicFickVariationalDiffusionBuilder::BUILDER
= new IsotropicFickVariationalDiffusionBuilder();

// constructor
IsotropicFickVariationalDiffusionBuilder::IsotropicFickVariationalDiffusionBuilder() {
  ModelDictionary::add("ISOTROPIC_FICK_VARIATIONAL_DIFFUSION",*this);
}

// build model
ConstitutiveModel* IsotropicFickVariationalDiffusionBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicFickVariationalDiffusion3D();
      break;
    case 2:
      return new IsotropicFickVariationalDiffusion2D();
      break;
    case 1:
      return new IsotropicFickVariationalDiffusion1D();
      break;
    default:
      return 0;
      break;
  }
}


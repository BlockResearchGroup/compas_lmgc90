/*
 *  $Id: IsotropicConductionPotential.cpp 126 2013-03-07 01:25:16Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicConductionPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicLinearConductionBuilder.
 */

// the instance
IsotropicLinearConductionBuilder const* IsotropicLinearConductionBuilder::BUILDER 
= new IsotropicLinearConductionBuilder();

// constructor
IsotropicLinearConductionBuilder::IsotropicLinearConductionBuilder() {
  ModelDictionary::add("ISOTROPIC_LINEAR_CONDUCTION",*this);
}

// build model
ConstitutiveModel* IsotropicLinearConductionBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicLinearConduction3D();
      break;
    case 2:
      return new IsotropicLinearConduction2D();
      break;
    case 1:
      return new IsotropicLinearConduction1D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 *  $Id: IsotropicLinConductionPotential.cpp 126 2013-03-07 01:25:16Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicLinConductionPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicLinVariationalConductionBuilder.
 */

// the instance
IsotropicLinVariationalConductionBuilder const* IsotropicLinVariationalConductionBuilder::BUILDER 
= new IsotropicLinVariationalConductionBuilder();

// constructor
IsotropicLinVariationalConductionBuilder::IsotropicLinVariationalConductionBuilder() {
  ModelDictionary::add("ISOTROPIC_LINEAR_VARIATIONAL_CONDUCTION",*this);
}

// build model
ConstitutiveModel* IsotropicLinVariationalConductionBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicLinVariationalConduction3D();
      break;
    case 2:
      return new IsotropicLinVariationalConduction2D();
      break;
    case 1:
      return new IsotropicLinVariationalConduction1D();
      break;
    default:
      return 0;
      break;
  }
}


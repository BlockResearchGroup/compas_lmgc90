/*
 *  $Id: IsotropicStdConductionPotential.cpp 126 2013-03-07 01:25:16Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "IsotropicStdConductionPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class IsotropicStdVariationalConductionBuilder.
 */

// the instance
IsotropicStdVariationalConductionBuilder const* IsotropicStdVariationalConductionBuilder::BUILDER 
= new IsotropicStdVariationalConductionBuilder();

// constructor
IsotropicStdVariationalConductionBuilder::IsotropicStdVariationalConductionBuilder() {
  ModelDictionary::add("ISOTROPIC_STANDARD_VARIATIONAL_CONDUCTION",*this);
}

// build model
ConstitutiveModel* IsotropicStdVariationalConductionBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicStdVariationalConduction3D();
      break;
    case 2:
      return new IsotropicStdVariationalConduction2D();
      break;
    case 1:
      return new IsotropicStdVariationalConduction1D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 *  $Id: J2DilatantPlasticity.cpp 233 2017-03-30 20:12:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "J2DilatantPlasticity.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class LinearIsotropicEllipticPlasticityBuilder.
 */

// the instance
LinearIsotropicEllipticPlasticityBuilder const* LinearIsotropicEllipticPlasticityBuilder::BUILDER
= new LinearIsotropicEllipticPlasticityBuilder();

// constructor
LinearIsotropicEllipticPlasticityBuilder::LinearIsotropicEllipticPlasticityBuilder() {
  ModelDictionary::add("LINEAR_ISOTROPIC_ELLIPTIC_PLASTICITY",*this);
}

// build model
ConstitutiveModel* LinearIsotropicEllipticPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearIsotropicEllipticPlasticity3D();
      break;
    case 2:
      return new LinearIsotropicEllipticPlasticity2D();
      break;
    case 1:
      return new LinearIsotropicEllipticPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 *  $Id: YeohPotential.cpp 199 2016-03-10 20:35:29Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "YeohPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class YeohBuilder.
 */

// the instance
YeohBuilder const* YeohBuilder::BUILDER = new YeohBuilder();

// constructor
YeohBuilder::YeohBuilder() {
  ModelDictionary::add("YEOH",*this);
}

// build model
ConstitutiveModel* YeohBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new Yeoh3D();
      break;
    case 2:
      return new Yeoh2D();
      break;
    case 1:
      return new Yeoh1D();
      break;
    default:
      return 0;
      break;
  }
}

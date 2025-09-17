/*
 *  $Id: StdMatlBifurcationCriterion.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "StdMatlBifurcationCriterion.h"

// local
#include <math/TensorAlgebra.h>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class StdMatlBifurcationCriterionBuilder.
 */

// the instance
StdMatlBifurcationCriterionBuilder const* StdMatlBifurcationCriterionBuilder::BUILDER 
= new StdMatlBifurcationCriterionBuilder();

// constructor
StdMatlBifurcationCriterionBuilder::StdMatlBifurcationCriterionBuilder() {
  CriterionDictionary::add("STANDARD_MATERIAL_BIFURCATION",*this);
}

// build model
MaterialCriterion* StdMatlBifurcationCriterionBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new StdMatlBifurcationCriterion<TensorAlgebra3D>();
      break;
    case 2:
      return new StdMatlBifurcationCriterion<TensorAlgebra3D>();
      break;
    case 1:
      return new StdMatlBifurcationCriterion<TensorAlgebra3D>();
      break;
    default:
      return 0;
      break;
  }
}



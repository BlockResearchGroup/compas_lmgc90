/*
 *  $Id: LinMatlBifurcationCriterion.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "LinMatlBifurcationCriterion.h"

// local
#include <math/TensorAlgebra.h>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class LinMatlBifurcationCriterionBuilder.
 */

// the instance
LinMatlBifurcationCriterionBuilder const* LinMatlBifurcationCriterionBuilder::BUILDER 
= new LinMatlBifurcationCriterionBuilder();

// constructor
LinMatlBifurcationCriterionBuilder::LinMatlBifurcationCriterionBuilder() {
  CriterionDictionary::add("LINEAR_MATERIAL_BIFURCATION",*this);
}

// build model
MaterialCriterion* LinMatlBifurcationCriterionBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinMatlBifurcationCriterion<TensorAlgebra3D>();
      break;
    case 2:
      return new LinMatlBifurcationCriterion<TensorAlgebra3D>();
      break;
    case 1:
      return new LinMatlBifurcationCriterion<TensorAlgebra3D>();
      break;
    default:
      return 0;
      break;
  }
}



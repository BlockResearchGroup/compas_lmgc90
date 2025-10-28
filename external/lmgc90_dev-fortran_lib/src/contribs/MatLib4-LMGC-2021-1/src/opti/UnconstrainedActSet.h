/*
 *  $Id: UnconstrainedActSet.h 130 2013-04-11 01:18:02Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_OPTI_UNCONSTRAINED_ACTIVE_SET_H
#define ZORGLIB_OPTI_UNCONSTRAINED_ACTIVE_SET_H

// config
#include <matlib_macros.h>

// local
#include "opti/OptiMethod.h"


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Unconstrained minimization method.
 * Uses a Newton method modified so that it is always a descent method.
 * Only bound constraints are taken into account, others are ignored.
 */
class UnconstrainedActSet : virtual public OptiMethod {

 protected:

  // map of active variables
  std::vector<bool> map;

  // set of active constraints
  std::vector<bool> set;

  // search direction
  ShortArray s;

 public:

  // constructor
  UnconstrainedActSet(OptiProblem&);

  // copy constructor
  UnconstrainedActSet(const UnconstrainedActSet&);

  // destructor
  virtual ~UnconstrainedActSet() {}

  // set associated problem
  void setProblem(OptiProblem&);

  // solve problem, starting from the given point
  double minimum(ShortArray&,double = DEFAULT_TOL)
   throw (OptimizationException);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

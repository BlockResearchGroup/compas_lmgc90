/*
 *  $Id: Unconstrained.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_OPTI_UNCONSTRAINED
#define ZORGLIB_OPTI_UNCONSTRAINED

// config
#include <matlib_macros.h>

// local
#include <opti/OptiMethod.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Unconstrained minimization method.
 * Only bound constraints are taken into account, others are ignored.
 * Uses a modified Newton method without active set (Bertsekas).
 */
class Unconstrained : virtual public OptiMethod {

 protected:

  // map of active variables
  std::vector<bool> map;

  // search direction
  ShortArray s;

 public:

  // constructor
  Unconstrained(OptiProblem&);

  // copy constructor
  Unconstrained(const Unconstrained&);

  // destructor
  virtual ~Unconstrained() {}

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

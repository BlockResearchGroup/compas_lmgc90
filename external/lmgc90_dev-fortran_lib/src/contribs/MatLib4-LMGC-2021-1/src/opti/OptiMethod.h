/*
 *  $Id: OptiMethod.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_OPTI_METHOD_H
#define ZORGLIB_OPTI_METHOD_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include <data/Exceptions.h>
#include <opti/OptiProblem.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Thrown when optimum could not be found.
 */
class OptimizationException : public ZException {
  
 public:
  
  // default constructor
  OptimizationException(const std::string& msg = "") : ZException(msg) {}
  
  // copy constructor
  OptimizationException(const OptimizationException& src) : ZException(src) {}
};


/**
 * Generic class for optimization methods.
 */
class OptiMethod {

 public:

  // default tolerance
  static const double DEFAULT_TOL;

#ifdef WITH_OPTI_DEBUG
  // debug levels
  static const unsigned int DBG_LOW = 1;
  static const unsigned int DBG_MID = 2;
  static const unsigned int DBG_TOP = 3;

 private:

  unsigned int dbg;
#endif

 protected:

  // associated problem
  OptiProblem* problem;

  // work arrays (NOT THREAD-SAFE!)
  ShortArray g;     /* gradient */
  ShortSqrMatrix H; /* Hessian matrix */
  ShortArray xx,gg; /* work arrays for line search */

  // constructor
  OptiMethod(OptiProblem&);
  
  // copy constructor
  OptiMethod(const OptiMethod&);
  
 public:

  // destructor
  virtual ~OptiMethod() {}

#ifdef WITH_OPTI_DEBUG
  // get debug level
  unsigned int debugLevel() const {return dbg;}

  // set debug level
  void setDebugLevel(unsigned int);
#endif

  // get associated problem dimension
  unsigned int dimension() const {
    return problem->dimension();
  }

  // get associated problem
  OptiProblem& getProblem() const {return *problem;}
  
  // set associated problem
  virtual void setProblem(OptiProblem&);

  // solve problem, starting from the given point
  virtual double minimum(ShortArray&,double = DEFAULT_TOL)
   throw (OptimizationException) = 0;

  // line search
  double lineSearch(double,double,ShortArray&,
                    const ShortArray&,double = DEFAULT_TOL)
   throw (OptimizationException);

  // Armijo's rule
  double armijoRule(double,ShortArray&,const ShortArray&,double,double);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

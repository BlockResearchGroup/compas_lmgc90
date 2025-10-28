/*
 *  $Id: OptiMethod.cpp 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "OptiMethod.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>


/**
 * Methods for class OptiMethod
 */

// default tolerance
const double OptiMethod::DEFAULT_TOL = 1.0e-8;

// constructor
OptiMethod::OptiMethod(OptiProblem& p) {
#ifdef WITH_OPTI_DEBUG
  dbg = 0;
#endif

  // set problem
  problem = &p;

  // allocate memory for work arrays
  unsigned int dim = p.dimension();
  g.resize(dim);
  H.resize(dim);
  xx.resize(dim);
  gg.resize(dim);
}

// copy constructor
OptiMethod::OptiMethod(const OptiMethod& src) {
#ifdef WITH_OPTI_DEBUG
  dbg = src.dbg;
#endif
  problem = src.problem;
  unsigned int dim = problem->dimension();
  g.resize(dim);
  H.resize(dim);
  xx.resize(dim);
  gg.resize(dim);
}

#ifdef WITH_OPTI_DEBUG
// set debug level
void OptiMethod::setDebugLevel(unsigned int l) {
  if (l <= DBG_TOP)
    dbg = l;
  else
    dbg = DBG_TOP;
}
#endif

// set associated problem
void OptiMethod::setProblem(OptiProblem& p) {

  // set problem
  problem = &p;

  // allocate memory for work arrays
  unsigned int dim = p.dimension();
  g.resize(dim);
  H.resize(dim);
  xx.resize(dim);
  gg.resize(dim);
}

// line search
double OptiMethod::lineSearch(double alpha00,double alphaMax,
                              ShortArray& x,const ShortArray& s,double tol)
 throw (OptimizationException) {
  unsigned int iter;
  double alpha0=0.0e0,alpha1=alpha00,alpha=1.0e0;
  double f,f0,f1,df,df0,df1;

  static const unsigned int ITMAX1=20,ITMAX2=50;
  static const double PRECISION = 1.0e-16;

  // get the function
  const OptiFunction& func = problem->objectiveFunction();

  // initial value of the function
  xx = x;
  f0 = f1 = func.value(xx,gg);
  df0 = df1 = innerProd(gg,s);

#ifdef WITH_OPTI_DEBUG
  if (debugLevel() >= DBG_MID) {
    std::cout << "SD:" << s << std::endl;
    std::cout << alpha0 << " " << alpha1 << " " << f0 << " " << f1 << std::endl;
  }
#endif

  // STEP 1: find initial interval
  bool hit = false;
  for (iter=0; iter < ITMAX1; iter++) {

    // check if the limit was hit
    if (alpha1 >= alphaMax) {
      hit = true;
      alpha1 = alphaMax;
    }

    // compute function and its derivative
    xx = x+alpha1*s;
    f1 = func.value(xx,gg);
    df1 = innerProd(gg,s);

#ifdef WITH_OPTI_DEBUG
    if (debugLevel() >= DBG_MID) {
      std::cout << alpha0 << " " << alpha1 << " " << f0 << " " << f1;
      std::cout << " " << df0 << " " << df1 << std::endl;
    }
#endif

    // have we found the correct interval?
    if (2*(f1-f0) >= tol*(std::fabs(f0)+std::fabs(f1)+tol)) break;

    // if not, but the bound was hit, is it a minimum?
    if (hit) {
      if (df1 < 0.0e0)
        return alpha1;
      else
        break;
    }
    else // if we're going up anyway ...
      if (df1 >= 0.0e0) break;

    // next interval
    alpha0  = alpha1;
    alpha1 += alpha1;

    f0 = f1; df0 = df1;
  }

  if (iter == ITMAX1)
    throw OptimizationException("line-search: phase 1");

  // refine interval with a few bissection steps
  for (; iter < ITMAX1; iter++) {

    // bissection
    alpha = 0.5*(alpha0+alpha1);

    // compute function and its derivative
    xx = x+alpha*s;
    f = func.value(xx,gg);
    df = innerProd(gg,s);

    // check new point
    if (f < f0 && df < 0.0e0) {
      alpha0 = alpha; f0 = f; df0 = df;
      break;
    }
    else 
      alpha1 = alpha; f1 = f; df1 = df;
  }

  // early termination test
  if (std::fabs(alpha1-alpha0) < PRECISION) return alpha1;

  // STEP 2: find minimum in interval
  for (; iter < ITMAX2; iter++) {

    // cubic interpolation
    double rho,val1,val2;
    val1 = 3*(f0-f1)/(alpha1-alpha0)+df0+df1;
    val2 = std::sqrt(val1*val1-df0*df1);
    rho = (df1+val2-val1)/(df1-df0+2*val2);
    alpha = alpha1-rho*(alpha1-alpha0);

    // compute function and its derivative
    xx = x+alpha*s;
    f = func.value(xx,gg);
    df = innerProd(gg,s);

#ifdef WITH_OPTI_DEBUG
    if (debugLevel() >= DBG_MID) {
      std::cout << alpha << " " << f << " " << df << std::endl;
    }
#endif
    
    // termination tests
    if (rho < tol) {
      if (f1 < f) alpha = alpha1;
      break;
    }
    else if ((1.0e0-rho) < tol) {
      if (f0 < f) alpha = alpha0;
      break;
    }

    if (2*std::fabs(f-f0) <= tol*(std::fabs(f)+std::fabs(f0)+tol)) {
      if (f0 < f) alpha = alpha0;
      break;
    }
    if (2*std::fabs(f-f1) <= tol*(std::fabs(f)+std::fabs(f1)+tol)) {
      if (f1 < f) alpha = alpha1;
      break;
    }

    // update interval
    if ((df > PRECISION) || (f > f0)) {
      alpha1 = alpha; f1 = f; df1 = df;
    }
    else {
      alpha0 = alpha; f0 = f; df0 = df;
    }
  }

  if (iter == ITMAX2)
    throw OptimizationException("line-search: phase 2");

  return alpha;
}

// Armijo's rule
double OptiMethod::armijoRule(double alpha0,ShortArray& x,const ShortArray& s,
                              double beta,double sigma) {
  unsigned int iter;
  double alpha = alpha0;
  double f,f0,df0,coef;

  static const unsigned int ITMAX=10;

  // get the function
  const OptiFunction& func = problem->objectiveFunction();

  // initial value of the function
  xx = x;
  f0 = func.value(xx,gg);
  df0 = innerProd(gg,s);
  coef = sigma*df0;

  // try successive reductions
  for (iter=0; iter < ITMAX; iter++) {

    // compute function
    xx = x+alpha*s;
    f = func.value(xx);

#ifdef WITH_OPTI_DEBUG
    if (debugLevel() >= DBG_MID) {
      std::cout << alpha << " " << f0 << " " << f << std::endl;
    }
#endif

    // check cost improvement
    if ((f0-f) >= coef*alpha) return alpha;

    // reduce step
    alpha *= beta;
  }
  return 0.0e0;
}

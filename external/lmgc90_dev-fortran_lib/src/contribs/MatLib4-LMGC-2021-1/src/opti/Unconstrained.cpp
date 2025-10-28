/*
 *  $Id: Unconstrained.cpp 130 2013-04-11 01:18:02Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "Unconstrained.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>


/**
 * Methods for class Unconstrained.
 */

// constructor
Unconstrained::Unconstrained(OptiProblem& p) 
 : OptiMethod(p) {

  // allocate memory for work arrays
  unsigned int dim = p.dimension();
  map.resize(dim);
  s.resize(dim);
}

// copy constructor
Unconstrained::Unconstrained(const Unconstrained& src) 
 : OptiMethod(src) {

  map = src.map;
  s = src.s;
}

// set associated problem
void Unconstrained::setProblem(OptiProblem& p) {

  // use parent method
  OptiMethod::setProblem(p);

  // allocate memory for work arrays
  unsigned int dim = p.dimension();
  map.resize(dim);
  s.resize(dim);
}

// solve problem, starting from the given point
double Unconstrained::minimum(ShortArray& x,double tol)
 throw (OptimizationException) {

  static const unsigned int ITMAX1 = 50,ITMAX2 = 13;
  static const double PRECISION = 1.0e-16;
  static const double TOLERANCE = 1.0e-08;

  unsigned int nDim = problem->dimension();
  unsigned int nLower = problem->nLower();
  unsigned int nUpper = problem->nUpper();

  // objective function
  const OptiFunction& func = problem->objectiveFunction();

  // initial value
  double f0 = func.value(x,g,H);
  double f1 = f0;

#ifdef WITH_OPTI_DEBUG
  if (debugLevel() >= DBG_LOW) {
    std::cout << "init" << "\t" << f1 << "\t" << x << std::endl;
  }
#endif

  // check norm of the initial gradient
  double gnorm = normL2(g);
  if (gnorm < PRECISION) return f0;

  // start Newton iterations
  unsigned int iter,iterMax = ITMAX1;
  double beta = 1.0e-3;
  for (iter=0; iter < iterMax; iter++) { // Newton loop

    // by default, all variables are active
    gg = -g;
    for (unsigned int i=0; i < nDim; i++) map[i] = true;
    
    // apply boundary constraints
    double test;
    for (unsigned int n=0; n < nLower; n++) {
      unsigned int i = problem->iLower(n);
      if (g[i] <= 0.e0) continue;
      test = x[i]-g[i];
      if (test <= problem->vLower(n))
        test = problem->vLower(n);
      double eps = std::fabs(x[i]-test);
      if (eps > TOLERANCE) eps = TOLERANCE;
      if ((x[i]-problem->vLower(n)) > eps) continue;
      if (H[i][i] > PRECISION)
        s[i] = gg[i]/std::fabs(H[i][i]);
      else
        s[i] = gg[i];
      map[i] = false;
    }
    for (unsigned int n=0; n < nUpper; n++) {
      unsigned int i = problem->iUpper(n);
      if (g[i] >= 0.e0) continue;
      test = x[i]-g[i];
      if (test >= problem->vUpper(n))
        test = problem->vUpper(n);
      double eps = std::fabs(x[i]-test);
      if (eps > TOLERANCE) eps = TOLERANCE;
      if ((problem->vUpper(n)-x[i]) > eps) continue;
      if (H[i][i] > PRECISION)
        s[i] = gg[i]/std::fabs(H[i][i]);
      else
        s[i] = gg[i];
      map[i] = false;
    }
    
    // get max diagonal term in H
    double hMax = 0.0e0;
    for (unsigned int i=0; i < nDim; i++) {
      if (!map[i]) continue;
      test = std::fabs(H[i][i]);
      hMax = (hMax > test) ? hMax:test;
    }
    
    // compute threshold
    double mu1,mu2;
    if (hMax > 1.0e0)
      mu1 = hMax*PRECISION;
    else
      mu1 = PRECISION;
    mu2 = beta*hMax;
    
    // compute corrected Newton direction:
    // if H is not strictly positive definite (i.e. > mu1),
    // a diagonal term mu2 is added
    bool modified = H.symSolve(s,gg,map,mu1,mu2);

#ifdef WITH_OPTI_DEBUG
    if (debugLevel() >= DBG_TOP) {
      std::cout << "s=" << s << std::endl;
    }
#endif
    
    // check norm of s
    double snorm = normL2(s);
    if (snorm < tol) break;

    // stepsize rule
    double gamma=0.2,sigma=1.e-3;
    double coef0=0.e0,coef1=0.e0;
    for (unsigned int i=0; i < nDim; i++) {
      if (map[i])
        coef0 -= g[i]*s[i];
      else
        coef1 += g[i]*x[i];
    }
    double alpha=1.0;
    unsigned int nIt;
    for (nIt=0; nIt < ITMAX2; nIt++) {

      xx = x+alpha*s;
      for (unsigned int n=0; n < nLower; n++) {
        unsigned int i = problem->iLower(n);
        if (xx[i] < problem->vLower(n)) xx[i] = problem->vLower(n);
      }
      for (unsigned int n=0; n < nUpper; n++) {
        unsigned int i = problem->iUpper(n);
        if (xx[i] > problem->vUpper(n)) xx[i] = problem->vUpper(n);
      }
      
      try {
        // compute function
        double f = func.value(xx);
      
#ifdef WITH_OPTI_DEBUG
        if (debugLevel() >= DBG_MID) {
          std::cout << alpha << " " << f0 << " " << f << std::endl;
        }
#endif
      
        // check improvement
        test = alpha*coef0+coef1;
        for (unsigned int i=0; i < nDim; i++) {
          if (map[i]) continue;
          test -= g[i]*xx[i];
        }
        if ((f0-f) >= sigma*test) break;      

        // reduce step
        alpha *= gamma;
      }
      catch (ZException) {
        // directly reduce step
        alpha *= gamma;
      }
    }
    //if (nIt == ITMAX2) break; // give up (test below should catch this)

    // do we actually move ?
    if (alpha*snorm < PRECISION) break;

    // compute new position and function value
    x += alpha*s;
    for (unsigned int n=0; n < nLower; n++) {
      unsigned int i = problem->iLower(n);
      if (x[i] < problem->vLower(n)) x[i] = problem->vLower(n);
    }
    for (unsigned int n=0; n < nUpper; n++) {
      unsigned int i = problem->iUpper(n);
      if (x[i] > problem->vUpper(n)) x[i] = problem->vUpper(n);
    }
    f1 = func.value(x,g,H);

#ifdef WITH_OPTI_DEBUG
    if (debugLevel() >= DBG_LOW) {
      if (modified)
        std::cout << iter << "(M)\t" << f1 << "\t" << alpha << "\t" << x << std::endl;
      else
        std::cout << iter << "(N)\t" << f1 << "\t" << alpha << "\t" << x << std::endl;
    }
#endif

    // check new gradient
    gnorm = normL2(g);
    if (gnorm < PRECISION) return f1;

    // termination test on function values
    if (2*std::fabs(f1-f0) < tol*(std::fabs(f0)+std::fabs(f1)+tol)) break;

    // update
    f0 = f1;

    // update value of beta
    if (modified) {
      if (alpha < 0.2)
        beta *= 5;
      else if (alpha > 0.9)
        beta /= 5;
    }
  }

  // check convergence
  if (iter == iterMax)
    throw OptimizationException("unconstrained minimization");

#ifdef WITH_OPTI_DEBUG
  if (debugLevel() >= DBG_LOW) {
    std::cout << x << std::endl;
    std::cout << "Clean exit" << std::endl;
  }
#endif
  
  return f1;
}

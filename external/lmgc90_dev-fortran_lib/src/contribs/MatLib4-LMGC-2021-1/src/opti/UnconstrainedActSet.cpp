/*
 *  $Id: UnconstrainedActSet.cpp 130 2013-04-11 01:18:02Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "UnconstrainedActSet.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>
// std C++ library
#include <limits>


/**
 * Methods for class UnconstrainedActSet.
 */

// constructor
UnconstrainedActSet::UnconstrainedActSet(OptiProblem& p) 
 : OptiMethod(p) {

  // allocate memory for work arrays
  unsigned int dim = p.dimension();
  map.resize(dim);
  s.resize(dim);

  unsigned int nLow = p.nLower();
  unsigned int nUpp = p.nUpper();
  set.resize(nLow+nUpp);
}

// copy constructor
UnconstrainedActSet::UnconstrainedActSet(const UnconstrainedActSet& src) 
 : OptiMethod(src) {

  map = src.map;
  set = src.set;
  s = src.s;
}

// set associated problem
void UnconstrainedActSet::setProblem(OptiProblem& p) {

  // use parent method
  OptiMethod::setProblem(p);

  // allocate memory for work arrays
  unsigned int dim = p.dimension();
  map.resize(dim);
  s.resize(dim);

  unsigned int nLow = p.nLower();
  unsigned int nUpp = p.nUpper();
  set.resize(nLow+nUpp);
}

// solve problem, starting from the given point
double UnconstrainedActSet::minimum(ShortArray& x,double tol)
 throw (OptimizationException) {

  static const unsigned int ITMAX = 20;
  static const double PRECISION = 1.0e-16;
  static const double THRSHLD = std::numeric_limits<double>::max();

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

  // initialize set of active constraints
  unsigned int m,n,nAct=0;
  for (n=0; n < nLower; n++) { // lower bounds
    if ((x[problem->iLower(n)] - problem->vLower(n)) < PRECISION) {
      set[n] = true;
      nAct++;
    }
    else
      set[n] = false;
  }
  for (m=0; m < nUpper; m++, n++) { // upper bounds
    if ((problem->vUpper(m) - x[problem->iUpper(m)]) < PRECISION) {
      set[n] = true;
      nAct++;
    }
    else
      set[n] = false;
  }

  // start Newton iterations
  unsigned int iter = 0;
  unsigned int iterMax = ITMAX+nLower+nUpper;
  unsigned int nLast = nLower+nUpper+1;
  double beta = 1.0e-3;
  while (iter < iterMax) { // loop on active constraints

    for (; iter < iterMax; iter++) { // Newton loop

      // by default, all variables are active
      gg = -g;
      for (unsigned int i=0; i < nDim; i++) map[i] = true;

      // apply boundary constraints
      for (n=0; n < nLower; n++) {
        if (!set[n]) continue;
        unsigned int i = problem->iLower(n);
        s[i] = 0.0e0;
        map[i] = false;
      }
      for (m=0; m < nUpper; m++, n++) {
        if (!set[n]) continue;
        unsigned int i = problem->iUpper(m);
        s[i] = 0.0e0;
        map[i] = false;
      }

      // get max diagonal term in H
      double hMax = 0.0e0;
      for (unsigned int i=0; i < nDim; i++) {
        if (!map[i]) continue;
        double test = std::fabs(H(i,i));
        hMax = (hMax > test) ? hMax:test;
      }

      // compute threshold
      double mu1,mu2;
      if (hMax > 1.0e0)
        mu1 = hMax*PRECISION;
      else
        mu1 = PRECISION;

      // compute corrected Newton direction:
      // if H is not strictly positive definite (i.e. > mu1),
      // a diagonal term mu2 is added
      bool modified = false;
      mu2 = beta*hMax;
      modified = H.symSolve(s,gg,map,mu1,mu2);
 
      // check norm of s
      double snorm = normL2(s);
      if (snorm < tol) break;

      // going down ?
      double df = innerProd(g,s);
      if (df > -PRECISION) break;

      // max step keeping us in the box
      unsigned int nMax = nLower+nUpper+1;
      double alpha,alphaMax = THRSHLD;
      for (n=0; n < nLower; n++) {
        if (set[n]) continue;
        unsigned int i = problem->iLower(n);
        if (s[i] > -PRECISION) continue;
        alpha = (problem->vLower(n) - x[i])/s[i];
        if (alpha < alphaMax) {
          nMax = n;
          alphaMax = alpha;
        }
      }
      for (m=0; n < nUpper; m++, n++) {
        if (set[n]) continue;
        unsigned int i = problem->iUpper(m);
        if (s[i] < PRECISION) continue;
        alpha = (problem->vUpper(m) - x[i])/s[i];
        if (alpha < alphaMax) {
          nMax = n;
          alphaMax = alpha;
        }
      }
      if (alphaMax < PRECISION) break;

#ifdef WITH_OPTI_DEBUG
      if (debugLevel() >= DBG_MID) {
        if (alphaMax < THRSHLD)
          std::cout << "alpha_max = " << alphaMax << std::endl;
      }
#endif

      // line-search along s
      double alpha0;
      if (modified) {
        alpha0 = 0.2;
        alpha = lineSearch(alpha0,alphaMax,x,s,tol);
      }
      else {
        alpha0 = (alphaMax < 1.0) ? alphaMax : 1.0;
        alpha = armijoRule(alpha0,x,s,0.2,1.e-3);
      }

      // did we hit a constraint ?
      bool hit = false;
      if ((alphaMax-alpha) < PRECISION) {
        hit = true;
        set[nMax] = true;
        nAct++;
        alpha = alphaMax;
#ifdef WITH_OPTI_DEBUG
        if (debugLevel() >= DBG_MID) {
          std::cout << "\tactivating constraint " << nMax << std::endl;
        }
#endif

        // try to avoid zig-zagging
        if (nMax != nLast) nLast = nLower+nUpper+1;
      }

      // update value of beta
      if (modified) {
        if (alpha < 0.2)
          beta *= 5;
        else if (alpha > 0.9)
          beta /= 5;
      }

      // do we actually move ?
      if (alpha*snorm < PRECISION) break;

      // compute new position and function value
      x += alpha*s;
      if (hit) {
        if (nMax < nLower)
          x[problem->iLower(nMax)] = problem->vLower(nMax);
        else
          x[problem->iUpper(nMax-nLower)] = problem->vUpper(nMax-nLower);
      }
      f1 = func.value(x,g,H);

#ifdef WITH_OPTI_DEBUG
      if (debugLevel() >= DBG_LOW) {
        std::cout << iter << "\t" << f1 << "\t" << x << std::endl;
      }
#endif

      // check new gradient
      gnorm = normL2(g);
      if (gnorm < PRECISION) return f1;

      // termination test on function values
      if (2*std::fabs(f1-f0) < tol*(std::fabs(f0)+std::fabs(f1)+tol)) break;

      // update
      f0 = f1;
    }

    // any constraints left ?
    if (nAct == 0) break;

    // compute maximal "multiplier"
    unsigned int nMax = nLower+nUpper+1;
    double lMax = 0.0e0;
    for (n=0; n < nLower; n++) {
      if (!set[n] || n == nLast) continue;
      unsigned int i = problem->iLower(n);
      if ((-g[i]) > lMax) {
        nMax = n;
        lMax = -g[i];
      }
    }
    for (m=0; m < nUpper; m++, n++) {
      if (!set[n] || n == nLast) continue;
      unsigned int i = problem->iUpper(m);
      if (g[i] > lMax) {
        nMax = n;
        lMax = g[i];
      }
    }

    // release the corresponding constraint
    if (lMax > PRECISION) {
      set[nMax] = false;
      nLast = nMax;
      nAct--;
#ifdef WITH_OPTI_DEBUG
      if (debugLevel() >= DBG_MID) {
        std::cout << "\treleasing constraint " << nMax << std::endl;
      }
#endif
    }
    else
      break;
  }

  // check convergence
  if (iter == iterMax)
    throw new OptimizationException("unconstrained minimization");

#ifdef WITH_OPTI_DEBUG
  if (debugLevel() >= DBG_LOW) {
    std::cout << x << std::endl;
    std::cout << "Clean exit" << std::endl;
  }
#endif
  
  return f1;
}

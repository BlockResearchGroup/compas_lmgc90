/*
 *  $Id: TabulatedFunction.cpp 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "TabulatedFunction.h"

// std C library
#include <cstring>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class TabulatedFunction.
 */

// default constructor
TabulatedFunction::TabulatedFunction(const std::string& str,unsigned int n)
 : Function(str) {

  if (n > 0) {
    nPts = n;
    // allocate memory
    x = new double[n];
    y = new double[n];
  }
  else {
    nPts = 0;
    x = y = 0;
  }
   
  lastIdx = 0;
}

// constructor
TabulatedFunction::TabulatedFunction(const std::string& str,
                                     unsigned int n,double *xx,double *yy)
 : Function(str) {

  if (n > 0) {
    nPts = n;
    // allocate memory
    x = new double[n];
    y = new double[n];
    std::memcpy(x,xx,n*sizeof(double));
    std::memcpy(y,yy,n*sizeof(double));
  }
  else {
    nPts = 0;
    x = y = 0;
  }

  lastIdx = 0;
}

// copy constructor
TabulatedFunction::TabulatedFunction(const TabulatedFunction& src)
 : Function(src) {

  nPts = src.nPts;
  if (nPts > 0) {
    // allocate memory
    x = new double[nPts];
    y = new double[nPts];
    std::memcpy(x,src.x,nPts*sizeof(double));
    std::memcpy(y,src.y,nPts*sizeof(double));
  }
  else {
    x = y = 0;
  }

  lastIdx = 0;
}

// destructor
TabulatedFunction::~TabulatedFunction() {
  if (x) delete [] x;
  if (y) delete [] y;
}

// resize
void TabulatedFunction::resize(unsigned int n) {
  if (n == nPts) return;

  if (x) delete [] x;
  if (y) delete [] y;

  nPts = n;
  if (nPts > 0) {
    // allocate memory
    x = new double[nPts];
    y = new double[nPts];
  }
  else {
    x = y = 0;
  }
  
  lastIdx = 0;
}

// set value
void TabulatedFunction::setPoint(unsigned int i,double xx,double yy) {
  if (i < nPts) {
    x[i] = xx;
    y[i] = yy;
  }
}

// get value
double TabulatedFunction::value(double u) {

  // quick return
  unsigned int idxMax = nPts-1;
  if (u <= x[0]) return y[0];
  if (u >= x[idxMax]) return y[idxMax];
  
  // find interval
  unsigned int idx=lastIdx;
  if (u > x[idx]) {
    while (idx < idxMax-1) {
      if (u <= x[idx+1]) break;
      idx++;
    }
  }
  else {
    while (idx > 0) {
      idx--;
      if (u > x[idx]) break;
    }
  }
  lastIdx = idx;

  // compute value
  return y[idx]+(u-x[idx])*(y[idx+1]-y[idx])/(x[idx+1]-x[idx]);
}

// get derivative
double TabulatedFunction::slope(double u) {
  
  // quick return
  unsigned int idxMax = nPts-1;
  if (u < x[0]) return 0.e0;
  if (u > x[idxMax]) return 0.e0;
  
  // find interval
  unsigned int idx=lastIdx;
  if (u > x[idx]) {
    while (idx < idxMax-1) {
      if (u <= x[idx+1]) break;
      idx++;
    }
  }
  else {
    while (idx > 0) {
      idx--;
      if (u > x[idx]) break;
    }
  }
  lastIdx = idx;

  // compute slope
  return (y[idx+1]-y[idx])/(x[idx+1]-x[idx]);
}

// get value and derivative
double TabulatedFunction::value(double u,double& df) {
  
  // quick return
  unsigned int idxMax = nPts-1;
  if (u < x[0]) {
    df = 0.e0; return y[0];
  }
  if (u > x[idxMax]) {
    df = 0.e0; return y[idxMax];
  }
  
  // find interval
  unsigned int idx=lastIdx;
  if (u > x[idx]) {
    while (idx < idxMax-1) {
      if (u <= x[idx+1]) break;
      idx++;
    }
  }
  else {
    while (idx > 0) {
      idx--;
      if (u > x[idx]) break;
    }
  }
  lastIdx = idx;

  // compute slope
  df = (y[idx+1]-y[idx])/(x[idx+1]-x[idx]);

  // compute value
  return y[idx]+(u-x[idx])*df;
}

// print-out
std::string TabulatedFunction::toString() const {
  O_STRING_STREAM os;
  os << "Tabulated function: " << getName() << " (" << nPts << " points)" << std::endl;
  for (unsigned int i=0; i < nPts; i++)
    os << "\t\t" << x[i] << "\t" << y[i] << std::endl;
#ifdef HAVE_SSTREAM
  return os.str();
#else
  return std::string(os.str(),os.pcount());
#endif
}

/*
 *  $Id: TabulatedFunction2.cpp 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "TabulatedFunction2.h"

// std C library
#include <cstring>

#include <iostream>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class TabulatedFunction2.
 */

// default constructor
TabulatedFunction2::TabulatedFunction2(const std::string& str,unsigned int n)
: Function(str), TabulatedFunction(str,n) {
  
  // allocate memory
  if (n > 0) 
    dy = new double[n];
  else
    dy = 0;
  
  updateSlopes = true;
}

// constructor
TabulatedFunction2::TabulatedFunction2(const std::string& str,
                                       unsigned int n,double *xx,double *yy)
: Function(str), TabulatedFunction(str,n,xx,yy) {
  
  computeSlopes();
  updateSlopes = true;
}

// copy constructor
TabulatedFunction2::TabulatedFunction2(const TabulatedFunction2& src)
: Function(src), TabulatedFunction(src) {
  
  if (nPts > 0) {
    // allocate memory
    dy = new double[nPts];
    std::memcpy(dy,src.dy,nPts*sizeof(double));
  }
  else {
    dy = 0;
  }
  updateSlopes = src.updateSlopes;
}

// destructor
TabulatedFunction2::~TabulatedFunction2() {
  if (dy) delete [] dy;
}

// resize
void TabulatedFunction2::resize(unsigned int n) {
  if (n == nPts) return;
  TabulatedFunction::resize(n);
  
  if (dy) delete [] dy;
  if (nPts > 0) {
    // allocate memory
    dy = new double[nPts];
  }
  else {
    dy = 0;
  }
  updateSlopes = true;
}

// set value
void TabulatedFunction2::setPoint(unsigned int i,double xx,double yy) {
  TabulatedFunction::setPoint(i,xx,yy);
  updateSlopes = true;
}

// compute slopes
void TabulatedFunction2::computeSlopes() {
  
  // we need at least 2 points
  if (nPts == 1) {
    dy[0] = 0.0e0;
  }
  else if (nPts > 1) {
    // simplified formulas at first and last points
    unsigned int idxMax = nPts-1;
    dy[0]      = (y[1]-y[0])/(x[1]-x[0]);
    dy[idxMax] = (y[idxMax]-y[idxMax-1])/(x[idxMax]-x[idxMax-1]);

    double dx10,dx20,dx21;
    double val0,val1,val2;
    if (nPts >= 3) {
      dx10 = x[1]-x[0];
      dx21 = x[2]-x[1];
      dx20 = x[2]-x[0];
      val0 = -dx21/(dx10*dx20);
      val1 = (dx21-dx10)/(dx21*dx10);
      val2 =  dx10/(dx21*dx20);
      dy[1] = val0*y[0]+val1*y[1]+val2*y[2];

      dx10 = x[idxMax-1]-x[idxMax-2];
      dx21 = x[idxMax]  -x[idxMax-1];
      dx20 = x[idxMax]  -x[idxMax-2];
      val0 = -dx21/(dx10*dx20);
      val1 = (dx21-dx10)/(dx21*dx10);
      val2 =  dx10/(dx21*dx20);
      dy[idxMax-1] = val0*y[idxMax-2]+val1*y[idxMax-1]+val2*y[idxMax];
    }
  
    // loop on other points, using a 5-point Newton polynomial interpolation
    double dx23,val00,val01,val02,val10,val11,val20,val3;
    for (unsigned int idx=2; idx < nPts-2; idx++) {
      dx20 = x[idx]-x[idx-2];
      dx21 = x[idx]-x[idx-1];
      dx23 = x[idx]-x[idx+1];
      val0 = (y[idx-1]-y[idx-2])/(x[idx-1]-x[idx-2]);
      val00 = (y[idx]-y[idx-1])/dx21;
      val1 = (val00-val0)/dx20;
      val01 = (y[idx+1]-y[idx])/(x[idx+1]-x[idx]);
      val10 = (val01-val00)/(x[idx+1]-x[idx-1]);
      val2 = (val10-val1)/(x[idx+1]-x[idx-2]);
      val02 = (y[idx+2]-y[idx+1])/(x[idx+2]-x[idx+1]);
      val11 = (val02-val01)/(x[idx+2]-x[idx]);
      val20 = (val11-val10)/(x[idx+2]-x[idx-1]);
      val3 = (val20-val2)/(x[idx+2]-x[idx-2]);
      dy[idx] = val0+val1*(dx20+dx21)+val2*dx20*dx21+val3*dx20*dx21*dx23;
    }
  }
  updateSlopes = false;
}

// get value
double TabulatedFunction2::value(double u) {
  
  if (updateSlopes) computeSlopes();

  // quick return
  unsigned int idxMax = nPts-1;
  if (u <= x[0]) return y[0]+(u-x[0])*dy[0];
  if (u >= x[idxMax]) return y[idxMax]+(u-x[idxMax])*dy[idxMax];
  
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
  double dx = x[idx+1]-x[idx];
  double z = (u-x[idx])/dx;
  if (z < 0.0e0) z = 0.0e0;
  if (z > 1.0e0) z = 1.0e0;
  double z2 = z*z;
  double dy0 = dx*dy[idx];
  double dy1 = dx*dy[idx+1];
  double val = y[idx+1]-y[idx];
  double A,B,C,D;
  A = y[idx];
  B = dy0;
  C =  3*val-2*dy0-dy1;
  D = -2*val+  dy0+dy1;
  return A+B*z+C*z2+D*z2*z;
}

// get derivative
double TabulatedFunction2::slope(double u) {
  
  if (updateSlopes) computeSlopes();

  // quick return
  unsigned int idxMax = nPts-1;
  if (u <= x[0]) return dy[0];
  if (u >= x[idxMax]) return dy[idxMax];
  
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
  double dx = x[idx+1]-x[idx];
  double dxi = 1.0e0/dx;
  double z = (u-x[idx])*dxi;
  if (z < 0.0e0) z = 0.0e0;
  if (z > 1.0e0) z = 1.0e0;
  double z2 = z*z;
  double val = (y[idx+1]-y[idx])*dxi;
  double A,B,C;
  A = dy[idx];
  B =  6*val-4*dy[idx]-2*dy[idx+1];
  C = -6*val+3*dy[idx]+3*dy[idx+1];
  return A+B*z+C*z2;
}

// get curvature
double TabulatedFunction2::curvature(double u) {
  
  if (updateSlopes) computeSlopes();
  
  // quick return
  unsigned int idxMax = nPts-1;
  if (u < x[0]) return 0.0e0;
  if (u > x[idxMax]) return 0.0e0;
  
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
  
  // compute curvature
  double dx = x[idx+1]-x[idx];
  double dxi = 1.0e0/dx;
  double z = (u-x[idx])*dxi;
  if (z < 0.0e0) z = 0.0e0;
  if (z > 1.0e0) z = 1.0e0;
  double val = (y[idx+1]-y[idx])*dxi;
  double A,B;
  A =   6*val-4*dy[idx]-2*dy[idx+1];
  B = -12*val+6*dy[idx]+6*dy[idx+1];
  return (A+B*z)*dxi;
}

// get value and derivative
double TabulatedFunction2::value(double u,double& df) {
  
  if (updateSlopes) computeSlopes();

  // quick return
  unsigned int idxMax = nPts-1;
  if (u <= x[0]) {
    df = dy[0]; 
    return y[0]+(u-x[0])*dy[0];
  }
  if (u >= x[idxMax]) {
    df = dy[idxMax]; 
    return y[idxMax]+(u-x[idxMax])*dy[idxMax];
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
  double dx = x[idx+1]-x[idx];
  double dxi = 1.0e0/dx;
  double z = (u-x[idx])*dxi;
  if (z < 0.0e0) z = 0.0e0;
  if (z > 1.0e0) z = 1.0e0;
  double z2 = z*z;
  double dy0 = dx*dy[idx];
  double dy1 = dx*dy[idx+1];
  double val = y[idx+1]-y[idx];
  double A,B,C,D;
  A = y[idx];
  B = dy0;
  C =  3*val-2*dy0-dy1;
  D = -2*val+  dy0+dy1;

  df = (B+2*C*z+3*D*z2)*dxi;
  
  // compute value
  return A+B*z+C*z2+D*z2*z;
}

// get value and derivatives
double TabulatedFunction2::value(double u,double& df,double& d2f) {
  
  if (updateSlopes) computeSlopes();

  // quick return
  unsigned int idxMax = nPts-1;
  if (u < x[0]) {
    d2f = 0.0e0;
    df = dy[0]; 
    return y[0];
  }
  if (u > x[idxMax]) {
    d2f = 0.0e0;
    df = dy[idxMax]; 
    return y[idxMax];
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
  
  // compute slope and curvature
  double dx = x[idx+1]-x[idx];
  double dxi = 1.0e0/dx;
  double coef = dxi*dxi;
  double z = (u-x[idx])*dxi;
  if (z < 0.0e0) z = 0.0e0;
  if (z > 1.0e0) z = 1.0e0;
  double z2 = z*z;
  double dy0 = dx*dy[idx];
  double dy1 = dx*dy[idx+1];
  double val = y[idx+1]-y[idx];
  double A,B,C,D;
  A = y[idx];
  B = dy0;
  C =  3*val-2*dy0-dy1;
  D = -2*val+  dy0+dy1;

  df = (B+2*C*z+3*D*z2)*dxi;
  d2f = (2*C+6*D*z)*coef;
  
  // compute value
  return A+B*z+C*z2+D*z2*z;
}

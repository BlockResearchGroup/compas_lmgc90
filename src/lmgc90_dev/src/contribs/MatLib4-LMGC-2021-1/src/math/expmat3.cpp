/*
 *  $Id: expmat3.cpp 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */

// std C library
#include <cmath>
#include <cstring>
// local
#include "MathUtils.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Compute the exponential of a 2x2 matrix and its derivatives
 * using a quasi-linear expression preserving volume changes.
 */
int expmat2_linear(const double A[],double expA[],double dExpA[][4],
                   double d2ExpA[][4][4],bool computeFirst,bool computeSecond) {
  
  static const double I2[4] = {1.,0.,0.,1.};
  static const double I4[4][4] = {{1.,0.,0.,0.},{0.,1.,0.,0.},
                                  {0.,0.,1.,0.},{0.,0.,0.,1.}};

  // approach exponential mapping by linear expression
  std::memcpy(expA,A,4*sizeof(double));
  expA[0] += 1.0e0;
  expA[3] += 1.0e0;
  
  // compute correction factor
  double val,expAInv[4];
  double trA = A[0]+A[3];
  if (computeFirst || computeSecond)
    val = tnvmat2(expA,expAInv);
  else
    val = detmat2(expA);
  if (std::fabs(val) < 1.0e-16) return 0;
  double coef = std::sqrt(std::exp(trA)/val);
  
  // derivatives
  int i,j,k,l,m,n,ij,kl,mn;
  if (computeFirst) {
    for (k=0, kl=0; k < 2; k++)
      for (l=0; l < 2; l++, kl++)
        for (i=0, ij=0; i < 2; i++)
          for (j=0; j < 2; j++, ij++)
            dExpA[kl][ij] = I4[ij][kl]+(expA[ij]*(I2[kl]-expAInv[kl]))*0.5;
  }
  if (computeSecond) {
    for (k=0, kl=0; k < 2; k++)
      for (l=0; l < 2; l++, kl++)
        for (m=0, mn=0; m < 2; m++)
          for (n=0; n < 2; n++, mn++)
            for (i=0, ij=0; i < 2; i++)
              for (j=0; j < 2; j++, ij++) 
                d2ExpA[kl][mn][ij] 
                  = coef*((I2[mn]-expAInv[mn])*dExpA[kl][ij]
                          +(I4[ij][mn]*(I2[kl]-expAInv[kl])
                           +expA[ij]*expAInv[k*2+n]*expAInv[m*2+l]))*0.5;
  }
  
  // apply correction factor
  for (ij=0; ij < 4; ij++) expA[ij] *= coef;
  if (computeFirst) {
    for (kl=0; kl < 4; kl++)
      for (ij=0; ij < 4; ij++) dExpA[kl][ij] *= coef;
  }

  return 1;
}


/*
 * Compute the exponential of a 3x3 matrix and its derivatives
 * using a quasi-linear expression preserving volume changes.
 */
int expmat3_linear(const double A[],double expA[],double dExpA[][9],
                   double d2ExpA[][9][9],bool computeFirst,bool computeSecond) {
  
  static const double I2[9] = {1.,0.,0.,0.,1.,0.,0.,0.,1.};
  static const double I4[9][9] = {{1.,0.,0.,0.,0.,0.,0.,0.,0.},
                                  {0.,1.,0.,0.,0.,0.,0.,0.,0.},
                                  {0.,0.,1.,0.,0.,0.,0.,0.,0.},
                                  {0.,0.,0.,1.,0.,0.,0.,0.,0.},
                                  {0.,0.,0.,0.,1.,0.,0.,0.,0.},
                                  {0.,0.,0.,0.,0.,1.,0.,0.,0.},
                                  {0.,0.,0.,0.,0.,0.,1.,0.,0.},
                                  {0.,0.,0.,0.,0.,0.,0.,1.,0.},
                                  {0.,0.,0.,0.,0.,0.,0.,0.,1.}};
  static const double THIRD = 1.e0/3.e0;
  
  // approach exponential mapping by linear expression
  std::memcpy(expA,A,9*sizeof(double));
  expA[0] += 1.0e0;
  expA[4] += 1.0e0;
  expA[8] += 1.0e0;
  
  // compute correction factor
  double coef,val,expAInv[9];
  if (computeFirst || computeSecond)
    val = tnvmat3(expA,expAInv);
  else
    val = detmat3(expA);
  if (std::fabs(val) < 1.0e-16) return 0;
#ifdef SLU
  if (val > 0.0e0)
    coef = std::pow(val,-THIRD);
  else
    coef = -std::pow(-val,-THIRD);
#else
  double trA = A[0]+A[4]+A[8];
  if (val > 0.0e0)
    coef = std::pow(std::exp(trA)/val,THIRD);
  else
    coef = -std::pow(-std::exp(trA)/val,THIRD);
#endif
  
  // derivatives
  int i,j,k,l,m,n,ij,kl,mn;
  if (computeFirst) {
    for (k=0, kl=0; k < 3; k++)
      for (l=0; l < 3; l++, kl++)
        for (i=0, ij=0; i < 3; i++)
          for (j=0; j < 3; j++, ij++)
#ifdef SLU
            dExpA[kl][ij] = I4[ij][kl]-THIRD*expA[ij]*expAInv[kl];
#else
            dExpA[kl][ij] = I4[ij][kl]+(expA[ij]*(I2[kl]-expAInv[kl]))*THIRD;
#endif
  }
  if (computeSecond) {
    for (k=0, kl=0; k < 3; k++)
      for (l=0; l < 3; l++, kl++)
        for (m=0, mn=0; m < 3; m++)
          for (n=0; n < 3; n++, mn++)
            for (i=0, ij=0; i < 3; i++)
              for (j=0; j < 3; j++, ij++) 
#ifdef SLU
                d2ExpA[kl][mn][ij] 
                  = coef*(-expAInv[mn]*dExpA[kl][ij]
                          +I4[ij][mn]*(I2[kl]-expAInv[kl])
                          +expA[ij]*expAInv[k*3+n]*expAInv[m*3+l])*THIRD;
#else
                d2ExpA[kl][mn][ij] 
                  = coef*((I2[mn]-expAInv[mn])*dExpA[kl][ij]
                          +I4[ij][mn]*(I2[kl]-expAInv[kl])
                          +expA[ij]*expAInv[k*3+n]*expAInv[m*3+l])*THIRD;
#endif
  }
  
  // apply correction factor
  for (ij=0; ij < 9; ij++) expA[ij] *= coef;
  if (computeFirst) {
    for (kl=0; kl < 9; kl++)
      for (ij=0; ij < 9; ij++) dExpA[kl][ij] *= coef;
  }
  
  return 1;
}

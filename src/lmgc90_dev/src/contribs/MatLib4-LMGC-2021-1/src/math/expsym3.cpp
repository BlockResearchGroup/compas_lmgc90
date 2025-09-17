/*
 *  $Id: expsym3.cpp 128 2013-03-09 01:55:32Z lstainier $
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
 * Compute the exponential of a 2x2 symmetric matrix and its derivatives
 * using a quasi-linear expression preserving volume changes.
 */
int expsym2_linear(const double A[],double expA[],double dExpA[][3],
                   double d2ExpA[][3][3],bool computeFirst,bool computeSecond) {
  
  static const double I2[3] = {1.,0.,1.};
  static const double I4[3][3] = {{1.,0.,0.},{0.,1.,0.},{0.,0.,1.}};

  // approach exponential mapping by linear expression
  std::memcpy(expA,A,3*sizeof(double));
  expA[0] += 1.0e0;
  expA[2] += 1.0e0;
  
  // compute correction factor
  double val,expAInv[3];
  double trA = A[0]+A[2];
  if (computeFirst || computeSecond)
    val = invsym2(expA,expAInv);
  else
    val = detsym2(expA);
  if (std::fabs(val) < 1.0e-16) return 0;
  double coef = std::sqrt(std::exp(trA)/val);
  
  // derivatives
  int i,j,k,l,m,n,ij,kl,mn,km,kn,lm,ln;
  if (computeFirst) {
    for (k=0, kl=0; k < 2; k++)
      for (l=0; l <= k; l++, kl++)
        for (i=0, ij=0; i < 2; i++)
          for (j=0; j <= i; j++, ij++)
            dExpA[kl][ij] = I4[ij][kl]+(expA[ij]*(I2[kl]-expAInv[kl]))*0.5;
  }
  if (computeSecond) {
    for (k=0, kl=0; k < 2; k++)
      for (l=0; l <= k; l++, kl++)
        for (m=0, mn=0; m < 2; m++)
          for (n=0; n <= m; n++, mn++)
            for (i=0, ij=0; i < 2; i++)
              for (j=0; j <= i; j++, ij++) {
                km = (k>m)?(k*(k+1)/2+m):(m*(m+1)/2+k);
                kn = (k>n)?(k*(k+1)/2+n):(n*(n+1)/2+k);
                lm = (l>m)?(l*(l+1)/2+m):(m*(m+1)/2+l);
                ln = (l>n)?(l*(l+1)/2+n):(n*(n+1)/2+l);
                d2ExpA[kl][mn][ij] 
                  = coef*((I2[mn]-expAInv[mn])*dExpA[kl][ij]
                          +(I4[ij][mn]*(I2[kl]-expAInv[kl])
                            +expA[ij]*0.5*(expAInv[km]*expAInv[ln]
                                          +expAInv[kn]*expAInv[lm])))*0.5;
	      }
  }
  
  // apply correction factor
  for (ij=0; ij < 3; ij++) expA[ij] *= coef;
  if (computeFirst) {
    for (kl=0; kl < 3; kl++)
      for (ij=0; ij < 3; ij++) dExpA[kl][ij] *= coef;
  }

  return 1;
}


/*
 * Compute the exponential of a 3x3 symmetric matrix and its derivatives
 * using a quasi-linear expression preserving volume changes.
 */
int expsym3_linear(const double A[],double expA[],double dExpA[][6],
                   double d2ExpA[][6][6],bool computeFirst,bool computeSecond) {
  
  static const double I2[6] = {1.,0.,1.,0.,0.,1.};
  static const double I4[6][6] = {{1.,0.,0.,0.,0.,0.},
                                  {0.,1.,0.,0.,0.,0.},
                                  {0.,0.,1.,0.,0.,0.},
                                  {0.,0.,0.,1.,0.,0.},
                                  {0.,0.,0.,0.,1.,0.},
                                  {0.,0.,0.,0.,0.,1.}};
  static const double THIRD = 1.e0/3.e0;
  
  // approach exponential mapping by linear expression
  std::memcpy(expA,A,6*sizeof(double));
  expA[0] += 1.0e0;
  expA[2] += 1.0e0;
  expA[5] += 1.0e0;
  
  // compute correction factor
  double coef,val,expAInv[6];
  double trA = A[0]+A[2]+A[5];
  if (computeFirst || computeSecond)
    val = invsym3(expA,expAInv);
  else
    val = detsym3(expA);
  if (std::fabs(val) < 1.0e-16) return 0;
  if (val > 0.0e0)
    coef = std::pow(std::exp(trA)/val,THIRD);
  else
    coef = -std::pow(-std::exp(trA)/val,THIRD);
  
  // derivatives
  int i,j,k,l,m,n,ij,kl,mn,km,kn,lm,ln;
  if (computeFirst) {
    for (k=0, kl=0; k < 3; k++)
      for (l=0; l < 3; l++, kl++)
        for (i=0, ij=0; i < 3; i++)
          for (j=0; j < 3; j++, ij++)
            dExpA[kl][ij] = I4[ij][kl]+(expA[ij]*(I2[kl]-expAInv[kl]))*THIRD;
  }
  if (computeSecond) {
    for (k=0, kl=0; k < 3; k++)
      for (l=0; l < 3; l++, kl++)
        for (m=0, mn=0; m < 3; m++)
          for (n=0; n < 3; n++, mn++)
            for (i=0, ij=0; i < 3; i++)
              for (j=0; j < 3; j++, ij++) {
                km = (k>m)?(k*(k+1)/2+m):(m*(m+1)/2+k);
                kn = (k>n)?(k*(k+1)/2+n):(n*(n+1)/2+k);
                lm = (l>m)?(l*(l+1)/2+m):(m*(m+1)/2+l);
                ln = (l>n)?(l*(l+1)/2+n):(n*(n+1)/2+l);
                d2ExpA[kl][mn][ij] 
                  = coef*((I2[mn]-expAInv[mn])*dExpA[kl][ij]
                          +(I4[ij][mn]*(I2[kl]-expAInv[kl])
                            +expA[ij]*0.5*(expAInv[km]*expAInv[ln]
                                          +expAInv[kn]*expAInv[lm])))*THIRD;
	      }
  }
  
  // apply correction factor
  for (ij=0; ij < 6; ij++) expA[ij] *= coef;
  if (computeFirst) {
    for (kl=0; kl < 6; kl++)
      for (ij=0; ij < 6; ij++) dExpA[kl][ij] *= coef;
  }
  
  return 1;
}

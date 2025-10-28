/*
 *  $Id: logmat3.cpp 129 2013-04-05 05:15:49Z lstainier $
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
 * Compute the logarithm of a 2x2 matrix and its derivatives
 * using a quasi-linear expression preserving volume changes.
 */
int logmat2_linear(const double A[],double logA[],double dLogA[][4],
                   double d2LogA[][4][4],bool computeFirst,bool computeSecond) {
  
  static const double I2[4] = {1.,0.,0.,1.};
  static const double I4[4][4] = {{1.,0.,0.,0.},{0.,1.,0.,0.},
                                  {0.,0.,1.,0.},{0.,0.,0.,1.}};

  // approach logarithmic mapping by linear expression
  std::memcpy(logA,A,4*sizeof(double));
  
  // compute correction factor
  double detA,coef,AInv[4],trAInv;
  double trA = A[0]+A[3];
  if (std::fabs(trA) < 1.0e-16) return 0;
  if (computeFirst || computeSecond) {
    detA = tnvmat2(A,AInv);
    trAInv = 1.0/detA;
  }
  else
    detA = detmat2(A);
  if (detA < 1.0e-16) return 0;
  coef = (std::log(detA)+3)/trA;
  int ij;
  for (ij=0; ij < 4; ij++) logA[ij] *= coef;
  
  // derivatives
  int i,j,k,l,m,n,kl,mn;
  if (computeFirst) {
    for (k=0, kl=0; k < 2; k++)
      for (l=0; l < 2; l++, kl++)
        for (i=0, ij=0; i < 2; i++)
          for (j=0; j < 2; j++, ij++)
            dLogA[kl][ij] = coef*I4[ij][kl]+trAInv*(A[ij]*AInv[kl]-logA[ij]*I2[kl]);
  }
  if (computeSecond) {
    for (k=0, kl=0; k < 2; k++)
      for (l=0; l < 2; l++, kl++)
        for (m=0, mn=0; m < 2; m++)
          for (n=0; n < 2; n++, mn++)
            for (i=0, ij=0; i < 2; i++)
              for (j=0; j < 2; j++, ij++)
                d2LogA[kl][mn][ij] = trAInv*(I4[ij][mn]*(AInv[kl]-coef*I2[kl])+I4[ij][kl]*AInv[mn]
                                             +trAInv*((logA[ij]*I2[mn]-A[ij]*AInv[mn])*I2[kl])
                                             -dLogA[kl][ij]*I2[mn]-A[ij]*AInv[k*2+n]*AInv[m*2+l]);
  }
                                             
  // finalize
  logA[0] -= 1.0;
  logA[3] -= 1.0;
                                             
  return 1;
}


/*
 * Compute the logarithm of a 3x3 matrix and its derivatives
 * using a quasi-linear expression preserving volume changes.
 */
int logmat3_linear(const double A[],double logA[],double dLogA[][9],
                   double d2LogA[][9][9],bool computeFirst,bool computeSecond) {
  
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

  // approach logarithmic mapping by linear expression
  std::memcpy(logA,A,9*sizeof(double));

  // compute correction factor
  double detA,coef,AInv[9],trAInv;
  double trA = A[0]+A[4]+A[8];
  if (std::fabs(trA) < 1.0e-16) return 0;
  if (computeFirst || computeSecond) {
    detA = tnvmat3(A,AInv);
    trAInv = 1.0/trA;
  }
  else
    detA = detmat3(A);
  if (detA < 1.0e-16) return 0;
  coef = (std::log(detA)+3)/trA;
  int ij;
  for (ij=0; ij < 9; ij++) logA[ij] *= coef;

  // derivatives
  int i,j,k,l,m,n,kl,mn;
  if (computeFirst) {
    for (k=0, kl=0; k < 3; k++)
      for (l=0; l < 3; l++, kl++)
        for (i=0, ij=0; i < 3; i++)
          for (j=0; j < 3; j++, ij++)
            dLogA[kl][ij] = coef*I4[ij][kl]+trAInv*(A[ij]*AInv[kl]-logA[ij]*I2[kl]);
  }
  if (computeSecond) {
    for (k=0, kl=0; k < 3; k++)
      for (l=0; l < 3; l++, kl++)
        for (m=0, mn=0; m < 3; m++)
          for (n=0; n < 3; n++, mn++)
            for (i=0, ij=0; i < 3; i++)
              for (j=0; j < 3; j++, ij++)
                d2LogA[kl][mn][ij] = trAInv*(I4[ij][mn]*(AInv[kl]-coef*I2[kl])+I4[ij][kl]*AInv[mn]
                                             +trAInv*((logA[ij]*I2[mn]-A[ij]*AInv[mn])*I2[kl])
                                             -dLogA[kl][ij]*I2[mn]-A[ij]*AInv[k*3+n]*AInv[m*3+l]);
  }
  
  // finalize
  logA[0] -= 1.0;
  logA[4] -= 1.0;
  logA[8] -= 1.0;
  
  return 1;
}

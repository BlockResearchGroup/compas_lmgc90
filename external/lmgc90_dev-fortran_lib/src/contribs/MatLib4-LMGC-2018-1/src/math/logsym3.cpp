/*
 *  $Id: logsym3.cpp 129 2013-04-05 05:15:49Z lstainier $
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
 * Compute the logarithm of a 2x2 symmetric matrix and its derivatives
 * using a quasi-linear expression preserving volume changes.
 */
int logsym2_linear(const double A[],double logA[],double dLogA[][3],
                   double d2LogA[][3][3],bool computeFirst,bool computeSecond) {
  
  static const double I2[3] = {1.,0.,1.};
  static const double I4[3][3] = {{1.,0.,0.},{0.,1.,0.},{0.,0.,1.}};

  // approach logarithmic mapping by linear expression
  std::memcpy(logA,A,3*sizeof(double));
  
  // compute correction factor
  double detA,coef,AInv[3],trAInv;
  double trA = logA[0]+logA[2];
  if (std::fabs(trA) < 1.0e-16) return 0;
  if (computeFirst || computeSecond) {
    detA = invsym2(A,AInv);
    trAInv = 1.0/trA;
  }
  else
    detA = detsym2(A);
  if (detA < 1.0e-16) return 0;
  coef = (std::log(detA)+3)/trA;
  int ij;
  for (ij=0; ij < 3; ij++) logA[ij] *= coef;
  
  // derivatives
  int i,j,k,l,m,n,kl,mn,km,kn,lm,ln;
  if (computeFirst) {
    for (k=0, kl=0; k < 2; k++)
      for (l=0; l <= k; l++, kl++)
        for (i=0, ij=0; i < 2; i++)
          for (j=0; j <= i; j++, ij++)
            dLogA[kl][ij] = coef*I4[ij][kl]+trAInv*(A[ij]*AInv[kl]-logA[ij]*I2[kl]);
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
                d2LogA[kl][mn][ij] = trAInv*(I4[ij][mn]*(AInv[kl]-coef*I2[kl])+I4[ij][kl]*AInv[mn]
                                             +trAInv*((logA[ij]*I2[mn]-A[ij]*AInv[mn])*I2[kl])
                                             -dLogA[kl][ij]*I2[mn]
                                             -0.5*A[ij]*(AInv[km]*AInv[ln]+AInv[kn]*AInv[lm]));
	      }
  }
  
  // finalize
  logA[0] -= 1.0;
  logA[2] -= 1.0;
  
  return 1;
}


/*
 * Compute the logarithm of a 3x3 symmetric matrix and its derivatives
 * using a quasi-linear expression preserving volume changes.
 */
int logsym3_linear(const double A[],double logA[],double dLogA[][6],
                   double d2LogA[][6][6],bool computeFirst,bool computeSecond) {
  
  static const double I2[6] = {1.,0.,1.,0.,0.,1.};
  static const double I4[6][6] = {{1.,0.,0.,0.,0.,0.},
                                  {0.,1.,0.,0.,0.,0.},
                                  {0.,0.,1.,0.,0.,0.},
                                  {0.,0.,0.,1.,0.,0.},
                                  {0.,0.,0.,0.,1.,0.},
                                  {0.,0.,0.,0.,0.,1.}};

  // approach logarithmic mapping by linear expression
  std::memcpy(logA,A,6*sizeof(double));
  
  // compute correction factor
  double detA,coef,AInv[6],trAInv;
  double trA = logA[0]+logA[2]+logA[5];
  if (std::fabs(trA) < 1.0e-16) return 0;
  if (computeFirst || computeSecond) {
    detA = invsym3(A,AInv);
    trAInv = 1.0/trA;
  }
  else
    detA = detsym3(A);
  if (detA < 1.0e-16) return 0;
  coef = (std::log(detA)+3)/trA;
  int ij;
  for (ij=0; ij < 6; ij++) logA[ij] *= coef;

  // derivatives
  int i,j,k,l,m,n,kl,mn,km,kn,lm,ln;
  if (computeFirst) {
    for (k=0, kl=0; k < 3; k++)
      for (l=0; l <= k; l++, kl++)
        for (i=0, ij=0; i < 3; i++)
          for (j=0; j <= i; j++, ij++)
            dLogA[kl][ij] = coef*I4[ij][kl]+trAInv*(A[ij]*AInv[kl]-logA[ij]*I2[kl]);
  }
  if (computeSecond) {
    for (k=0, kl=0; k < 3; k++)
      for (l=0; l <= k; l++, kl++)
        for (m=0, mn=0; m < 3; m++)
          for (n=0; n <= m; n++, mn++)
            for (i=0, ij=0; i < 3; i++)
              for (j=0; j <= i; j++, ij++) {
                km = (k>m)?(k*(k+1)/2+m):(m*(m+1)/2+k);
                kn = (k>n)?(k*(k+1)/2+n):(n*(n+1)/2+k);
                lm = (l>m)?(l*(l+1)/2+m):(m*(m+1)/2+l);
                ln = (l>n)?(l*(l+1)/2+n):(n*(n+1)/2+l);
                d2LogA[kl][mn][ij] = trAInv*(I4[ij][mn]*(AInv[kl]-coef*I2[kl])+I4[ij][kl]*AInv[mn]
                                             +trAInv*((logA[ij]*I2[mn]-A[ij]*AInv[mn])*I2[kl])
                                             -dLogA[kl][ij]*I2[mn]
                                             -0.5*A[ij]*(AInv[km]*AInv[ln]+AInv[kn]*AInv[lm]));
	      }
  }
  
  // finalize
  logA[0] -= 1.0;
  logA[2] -= 1.0;
  logA[5] -= 1.0;
  
  return 1;
}

/*
 *  $Id: eigsym.cpp 143 2014-04-18 07:12:34Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "eigsym.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>
#include <cstring>
// local headers
#ifdef MATLIB_USE_LAPACK
#include "lapack.h"
#endif


/**
 * Compute eigenvalues and eigen vectors for a 3x3 symmetric matrix.
 */
int MATLIB_NAMESPACE eigsym3(const double A[],double eigVal[],
			     double eigVec[][3]) {

  double M[6],V[9];

  // copy A to M
  std::memcpy(M,A,6*sizeof(double));

  // solve eigen problem
  int test;
#ifndef MATLIB_USE_LAPACK
  double wrk1[3],wrk2[3];
  test = jacobi(3,3,M,eigVal,V,wrk1,wrk2);
#else
  char jobz='V',uplo='U';
  LAPACK_INTEGER size=3,ierr;
  LAPACK_DOUBLE work[9];
  FORTRAN(dspev)(&jobz,&uplo,&size,M,eigVal,V,&size,work,&ierr);
  test = !ierr;
#endif
  if (!test) return test;

  // get eigenvectors
  double *pV=V;
  for (int i=0; i < 3; i++, pV+=3) {
    std::memcpy(eigVec[i],pV,3*sizeof(double));
  }

  return 1;
}


/**
 * Compute eigenvalues and eigen vectors for a 2x2 symmetric matrix.
 */
int MATLIB_NAMESPACE eigsym2(const double A[],double eigVal[],
			     double eigVec[][2]) {

  double M[3],V[4];

  // copy A to M
  std::memcpy(M,A,3*sizeof(double));

  // solve eigen problem
  int test;
#ifndef MATLIB_USE_LAPACK
  double wrk1[2],wrk2[2];
  test = jacobi(2,2,M,eigVal,V,wrk1,wrk2);
#else
  char jobz='V',uplo='U';
  LAPACK_INTEGER size=2,ierr;
  LAPACK_DOUBLE work[6];
  FORTRAN(dspev)(&jobz,&uplo,&size,M,eigVal,V,&size,work,&ierr);
  test = !ierr;
#endif
  if (!test) return test;

  // get eigenvectors
  double *pV=V;
  for (int i=0; i < 2; i++, pV+=2) {
    std::memcpy(eigVec[i],pV,2*sizeof(double));
  }

  return 1;
}

/**
 * Solve eigen problem for a symmetric matrix (Jacobi algorithm)
 */
int MATLIB_NAMESPACE jacobi(int n,int nm,double *A,double *d,double *V,
			    double *b,double *z)
{
  static const double PREC = 1.e-16;
  static const int NSWMAX = 50;

  int i,j,k,ii,ij,ik,ki=0,kj=0,kki,kkj,ival,jval;
  int nrot,nsweep;
  double c,g,h,s,t,tau,theta,tresh,sum;

  // initialize eigenvectors
  for (j=0,jval=0; j < n; j++,jval+=nm) {
    for (i=0; i < n; i++) V[jval+i] = 0.e0;
    V[jval+j] = 1.e0;
  }

  // initialize b and d to the diagonal of A
  for (i=0, ii=0; i < n; i++, ii+=(i+1)) {
    b[i] = d[i] = A[ii];
    z[i] = 0.e0;
  }

  // begin sweeping process
  nrot = 0;
  for (nsweep=0; nsweep < NSWMAX; nsweep++) {

    /* Sum off-diagonal elements */
    sum = 0.e0;
    for (i=0, ij=0; i < n; i++, ij++)
      for (j=0; j < i; j++, ij++)
        sum += std::fabs(A[ij]);

    if (sum < PREC) goto SORT;

    // compute a treshold on the first 3 sweeps
    if (nsweep < 4)
      tresh = 0.2*sum/(n*n);
    else
      tresh = 0.e0;

    // browse the matrix
    for (i=0, ij=0, ival=0; i < n; i++, ij++, ival+=i)
      for (j=0, jval=0; j < i; j++, ij++, jval+=j) {

        // test off-diagonal element
        g = 100.*std::fabs(A[ij]);
        if ((nsweep > 4) 
            && (std::fabs(d[i]+g) == std::fabs(d[i])) 
            && (std::fabs(d[j]+g) == std::fabs(d[j]))) {

          A[ij] = 0.e0;
        }
        else if (std::fabs(A[ij]) > tresh) {

          // compute the rotation
          h = d[j]-d[i];
          if ((std::fabs(h)+g) == std::fabs(h))
            t = A[ij]/h;
          else {
            theta = 0.5*h/A[ij];
            t = 1.e0/(std::fabs(theta)+std::sqrt(1.0+theta*theta));
            if (theta < 0.e0) t = -t;
          }
          c = 1.e0/std::sqrt(1.+t*t);
          s = t*c;
          tau = s/(1.e0+c);

          // rotate rows i and j
          h = t*A[ij];
          z[i] -= h;
          z[j] += h;
          d[i] -= h;
          d[j] += h;
          A[ij]  = 0.e0;

          for (k=0,kki=i*nm,kkj=j*nm; k < n; k++, kki++, kkj++) {

            g = V[kki];
            h = V[kkj];
            V[kki] = g-s*(h+g*tau);
            V[kkj] = h+s*(g-h*tau);

            ki = (k<=i)?(ival+k):(ki+k);
            kj = (k<=j)?(jval+k):(kj+k);
            if (k == i || k == j) continue;

            g = A[ki];
            h = A[kj];
            A[ki] = g-s*(h+g*tau);
            A[kj] = h+s*(g-h*tau);
          }

          nrot++;
        }
      }

    // update d and reinitialize z
    for (i=0; i < n; i++) {
      b[i] += z[i];
      d[i] = b[i];
      z[i] = 0.e0;
    }
  }

  return 0;

SORT:

  /* Re-order eigenvalues by descending order */

  for (j=0, jval=0; j < n; j++, jval+=nm) {

    t = d[j];
    k = j;

    for (i=j; i < n; i++)
      if (d[i] >= t) {t = d[i]; k=i;}

    if (k == j) continue;

    t    = d[j];
    d[j] = d[k];

    d[k] = t;

    for (i=0, ij=jval, ik=k*nm; i < n; i++, ij++, ik++) {

      t     = V[ik];
      V[ik] = V[ij];
      V[ij] = t;
    }
  }

  return nrot+1;
}


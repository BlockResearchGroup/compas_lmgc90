/*
 *  $Id: expsym1.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */

// std C library
#include <cstring>
// local
#include "MathUtils.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

static void symsym2(const double*,const double*,double*);
static void symsym3(const double*,const double*,double*);

/*
 * Compute the exponential of a 2x2 symmetric matrix and its derivatives
 * using truncated series
 */
int expsym2_series(const double A[],double expA[],double dExpA[][3],double d2ExpA[][3][3],
                   bool computeFirst,bool computeSecond) {

  static const int ITMAX = 25;
  static const double PREC = 1.0e-16;

  int i,j,k,l,m,n,ij,kl,mn;
  double X[3] = {1.e0, 0.e0, 1.e0};
  double dX[3][3],d2X[3][3][3],Y[3],dY[3][3],d2Y[3][3][3];

  /*
   * Initialization
   */
  std::memcpy(expA,X,3*sizeof(double));

  if (computeFirst || computeSecond) {
    std::memset(dX,0,9*sizeof(double));
    dX[0][0] = dX[2][2] = 1.0e0;
    dX[1][1] = 0.5e0;
    std::memcpy(dExpA,dX,9*sizeof(double));
  }

  if (computeSecond) {
    std::memset(d2X,0,27*sizeof(double));
    for (k=0, kl=0; k < 2; k++)
      for (l=0; l <= k; l++, kl++)
        for (m=0, mn=0; m < 2; m++)
          for (n=0; n <= m; n++, mn++)
            for (i=0, ij=0; i < 2; i++)
              for (j=0; j <= i; j++, ij++) {
                if (i == k && l == m && j == n)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == m && k == n && j == l)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == l && k == m && j == n)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == m && l == n && j == k)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == k && l == n && j == m)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == n && k == m && j == l)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == l && k == n && j == m)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == n && l == m && j == k)
                  d2X[kl][mn][ij] += 0.125e0;
              }
    std::memcpy(d2ExpA,d2X,27*sizeof(double));
  }

  /*
   * Start the loop for the series
   */
  for (int iter=0; iter < ITMAX; iter++) {
    double coef = 1.e0/(iter+1);
    double error = 0.e0;
    double norm;

    // second derivative
    if (computeSecond && iter >= 2) {
      std::memcpy(d2Y,d2X,27*sizeof(double));

      for (k=0, kl=0; k < 2; k++)
        for (l=0; l <= k; l++, kl++) 
          for (m=0, mn=0; m < 2; m++)
            for (n=0; n <= m; n++, mn++) {

              // the sum of the three contributions is symmetric,
              // but not the contribution themselves !
              symsym2(d2Y[kl][mn],A,d2X[kl][mn]);

              int ii,kk=k*(k+1)/2,mm=m*(m+1)/2;
              for (i=0, ii=0; i < 2; i++, ii+=i) {
                if (i >= m) {
                  d2X[kl][mn][ii+n] += 0.5*dX[kl][ii+m]; 
                  d2X[kl][mn][ii+m] += 0.5*dX[kl][ii+n];
                }
                else if (i >= n) {
                  d2X[kl][mn][ii+n] += 0.5*dX[kl][mm+i];
                }
              } 

              for (i=0, ii=0; i < 2; i++, ii+=i) {
                if (i >= k) {
                  d2X[kl][mn][ii+l] += 0.5*dX[mn][ii+k]; 
                  d2X[kl][mn][ii+k] += 0.5*dX[mn][ii+l];
                }
                else if (i >= l) {
                  d2X[kl][mn][ii+l] += 0.5*dX[mn][kk+i];
                }
              } 
            }
      mulvec(coef,d2X[0][0],d2X[0][0],27);
      if (computeSecond) addvec(d2ExpA[0][0],d2X[0][0],d2ExpA[0][0],27);
    
      norm = nrmvec1(d2X[0][0],27);
      error = (norm > error) ? norm:error;
    }

    // first derivative
    if ((computeFirst || computeSecond) && iter >= 1) {
      std::memcpy(dY,dX,9*sizeof(double));

      for (k=0, kl=0; k < 2; k++)
        for (l=0; l <= k; l++, kl++) {

          // the sum of the two contributions is symmetric,
          // but not the contribution themselves !
          symsym2(dY[kl],A,dX[kl]);

          int ii,kk = k*(k+1)/2;
          for (i=0, ii=0; i < 2; i++, ii+=i) {
            if (i >= k) {
              dX[kl][ii+l] += 0.5*X[ii+k]; 
              dX[kl][ii+k] += 0.5*X[ii+l];
            }
            else if (i >= l) {
              dX[kl][ii+l] += 0.5*X[kk+i];
            }
          } 
        }  
      mulvec(coef,dX[0],dX[0],9);
      if (computeFirst) addvec(dExpA[0],dX[0],dExpA[0],9);

      norm = nrmvec1(dX[0],9);
      error = (norm > error) ? norm:error;
    }

    // exponential
    std::memcpy(Y,X,3*sizeof(double));

    symsym2(Y,A,X); // X = Y*A (X still symmetric in this case)
    mulvec(coef,X,X,3);
    addvec(expA,X,expA,3);

    norm = nrmvec1(X,3);
    error = (norm > error) ? norm:error;

    // check convergence
    if (error < PREC) return iter+1;
  }

  return 0;
}


/*
 * Compute the exponential of a 3x3 symmetric matrix and its derivatives
 * using truncated series
 */
int expsym3_series(const double A[],double expA[],double dExpA[][6],double d2ExpA[][6][6],
                   bool computeFirst,bool computeSecond) {

  static const int ITMAX = 25;
  static const double PREC = 1.e-16;

  int i,j,k,l,m,n,ij,kl,mn;
  double X[6] = {1.e0, 0.e0, 1.e0, 0.e0, 0.e0, 1.e0};
  double dX[6][6],d2X[6][6][6],Y[6],dY[6][6],d2Y[6][6][6];

  /*
   * Initialization
   */
  std::memcpy(expA,X,6*sizeof(double));

  if (computeFirst || computeSecond) {
    std::memset(dX,0,36*sizeof(double));
    dX[0][0] = dX[2][2] = dX[5][5] = 1.0e0;
    dX[1][1] = dX[3][3] = dX[4][4] = 0.5e0;
    std::memcpy(dExpA,dX,36*sizeof(double));
  }

  if (computeSecond) {
    std::memset(d2X,0,216*sizeof(double));
    for (k=0, kl=0; k < 3; k++)
      for (l=0; l <= k; l++, kl++)
        for (m=0, mn=0; m < 3; m++)
          for (n=0; n <= m; n++, mn++)
            for (i=0, ij=0; i < 3; i++)
              for (j=0; j <= i; j++, ij++) {
                if (i == k && l == m && j == n)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == m && k == n && j == l)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == l && k == m && j == n)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == m && l == n && j == k)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == k && l == n && j == m)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == n && k == m && j == l)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == l && k == n && j == m)
                  d2X[kl][mn][ij] += 0.125e0;
                if (i == n && l == m && j == k)
                  d2X[kl][mn][ij] += 0.125e0;
              }
    std::memcpy(d2ExpA,d2X,216*sizeof(double));
  }

  /*
   * Start the loop for the series
   */
  for (int iter=0; iter < ITMAX; iter++) {
    double coef = 1.e0/(iter+1);
    double error = 0.e0;
    double norm;

    // second derivative
    if (computeSecond && iter >= 2) {
      std::memcpy(d2Y,d2X,216*sizeof(double));

      for (k=0, kl=0; k < 3; k++)
        for (l=0; l <= k; l++, kl++) 
          for (m=0, mn=0; m < 3; m++)
            for (n=0; n <= m; n++, mn++) {

              // the sum of the three contributions is symmetric,
              // but not the contribution themselves !
              symsym3(d2Y[kl][mn],A,d2X[kl][mn]);

              int ii,kk=k*(k+1)/2,mm=m*(m+1)/2;
              for (i=0, ii=0; i < 3; i++, ii+=i) {
                if (i >= m) {
                  d2X[kl][mn][ii+n] += 0.5*dX[kl][ii+m]; 
                  d2X[kl][mn][ii+m] += 0.5*dX[kl][ii+n];
                }
                else if (i >= n) {
                  d2X[kl][mn][ii+n] += 0.5*dX[kl][mm+i];
                }
              } 

              for (i=0, ii=0; i < 3; i++, ii+=i) {
                if (i >= k) {
                  d2X[kl][mn][ii+l] += 0.5*dX[mn][ii+k]; 
                  d2X[kl][mn][ii+k] += 0.5*dX[mn][ii+l];
                }
                else if (i >= l) {
                  d2X[kl][mn][ii+l] += 0.5*dX[mn][kk+i];
                }
              } 
            }
      mulvec(coef,d2X[0][0],d2X[0][0],216);
      if (computeSecond) addvec(d2ExpA[0][0],d2X[0][0],d2ExpA[0][0],216);
    
      norm = nrmvec1(d2X[0][0],216);
      error = (norm > error) ? norm:error;
    }

    // first derivative
    if ((computeFirst || computeSecond) && iter >= 1) {
      std::memcpy(dY,dX,36*sizeof(double));

      for (k=0, kl=0; k < 3; k++)
        for (l=0; l <= k; l++, kl++) {

          // the sum of the two contributions is symmetric,
          // but not the contribution themselves !
          symsym3(dY[kl],A,dX[kl]);

          int ii,kk = k*(k+1)/2;
          for (i=0, ii=0; i < 3; i++, ii+=i) {
            if (i >= k) {
              dX[kl][ii+l] += 0.5*X[ii+k]; 
              dX[kl][ii+k] += 0.5*X[ii+l];
            }
            else if (i >= l) {
              dX[kl][ii+l] += 0.5*X[kk+i];
            }
          } 
        }  
      mulvec(coef,dX[0],dX[0],36);
      if (computeFirst) addvec(dExpA[0],dX[0],dExpA[0],36);

      norm = nrmvec1(dX[0],36);
      error = (norm > error) ? norm:error;
    }

    // exponential
    std::memcpy(Y,X,6*sizeof(double));

    symsym3(Y,A,X); // X = Y*A (X still symmetric in this case)
    mulvec(coef,X,X,6);
    addvec(expA,X,expA,6);

    norm = nrmvec1(X,6);
    error = (norm > error) ? norm:error;

    // check convergence
    if (error < PREC) return iter+1;
  }

  return 0;
}

/**
 * Multiplication of two symmetric matrices,
 * only the lower triangular part of the result is stored
 * (result not necessarily symmetric)
 */
void symsym2(const double *A,const double *B,double *C) {
  C[0] = A[0]*B[0]+A[1]*B[1];
  C[1] = A[1]*B[0]+A[2]*B[1];
  C[2] = A[1]*B[1]+A[2]*B[2];
}

void symsym3(const double *A,const double *B,double *C) {
  C[0] = A[0]*B[0]+A[1]*B[1]+A[3]*B[3];
  C[1] = A[1]*B[0]+A[2]*B[1]+A[4]*B[3];
  C[2] = A[1]*B[1]+A[2]*B[2]+A[4]*B[4];
  C[3] = A[3]*B[0]+A[4]*B[1]+A[5]*B[3];
  C[4] = A[3]*B[1]+A[4]*B[2]+A[5]*B[4];
  C[5] = A[3]*B[3]+A[4]*B[4]+A[5]*B[5];
}


/*
 *  $Id: expmat1.cpp 124 2013-01-11 16:41:33Z lstainier $
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

/*
 * Compute the exponential of a 2x2 matrix and its derivatives
 * using truncated series
 */
int expmat2_series(const double A[],double expA[],double dExpA[][4],double d2ExpA[][4][4],
                   bool computeFirst,bool computeSecond) {

  static const int ITMAX = 25;
  static const double PREC = 1.0e-16;

  int i,j,k,l,m,n,ik,il,im,in,kl,mn;
  double X[4] = {1.e0, 0.e0, 0.e0, 1.e0};
  double dX[4][4],d2X[4][4][4],Y[4],dY[4][4],d2Y[4][4][4];

  /*
   * Initialization
   */
  std::memcpy(expA,X,4*sizeof(double));

  if (computeFirst || computeSecond) {
    std::memset(dX,0,16*sizeof(double));
    dX[0][0] = dX[1][1] = dX[2][2] = dX[3][3] = 1.0e0;
    std::memcpy(dExpA,dX,16*sizeof(double));
  }

  if (computeSecond) {
    std::memset(d2X,0,64*sizeof(double));
    for (i=0; i < 2; i++)
      for (j=0; j < 2; j++)
        for (k=0; k < 2; k++) {
          d2X[i*2+k][k*2+j][i*2+j] += 0.5e0;
          d2X[j*2+k][i*2+j][i*2+k] += 0.5e0;
        }
    std::memcpy(d2ExpA,d2X,64*sizeof(double));
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
      std::memcpy(d2Y,d2X,64*sizeof(double));

      for (k=0, kl=0; k < 2; k++)
        for (l=0; l < 2; l++, kl++) 
          for (m=0, mn=0; m < 2; m++)
            for (n=0; n < 2; n++, mn++) mulmat2(d2Y[kl][mn],A,d2X[kl][mn]);
      for (i=0, im=0; i < 2; i++) 
        for (m=0, mn=0; m < 2; m++, im++)
          for (n=0, in=i*2; n < 2; n++, mn++, in++)
            for (k=0, kl=0; k < 2; k++)
              for (l=0; l < 2; l++, kl++) d2X[kl][mn][in] += dX[kl][im];
      for (i=0, ik=0; i < 2; i++)
        for (k=0, kl=0; k < 2; k++, ik++)
          for (l=0, il=i*2; l < 2; l++, kl++, il++) 
            for (m=0, mn=0; m < 2; m++)
              for (n=0; n < 2; n++, mn++) d2X[kl][mn][il] += dX[mn][ik];
      mulvec(coef,d2X[0][0],d2X[0][0],64);
      if (computeSecond) addvec(d2ExpA[0][0],d2X[0][0],d2ExpA[0][0],64);
    
      norm = nrmvec1(d2X[0][0],64);
      error = (norm > error) ? norm:error;
    }

    // first derivative
    if ((computeFirst || computeSecond) && iter >= 1) {
      std::memcpy(dY,dX,16*sizeof(double));

      for (k=0, kl=0; k < 2; k++)
        for (l=0; l < 2; l++, kl++) mulmat2(dY[kl],A,dX[kl]);
      for (i=0, ik=0; i < 2; i++)
        for (k=0, kl=0; k < 2; k++, ik++)
          for (l=0, il=i*2; l < 2; l++, kl++, il++) dX[kl][il] += X[ik];   
      mulvec(coef,dX[0],dX[0],16);
      if (computeFirst) addvec(dExpA[0],dX[0],dExpA[0],16);

      norm = nrmvec1(dX[0],16);
      error = (norm > error) ? norm:error;
    }

    // exponential
    std::memcpy(Y,X,4*sizeof(double));

    mulmat2(Y,A,X);
    mulvec(coef,X,X,4);
    addvec(expA,X,expA,4);

    norm = nrmvec1(X,4);
    error = (norm > error) ? norm:error;

    // check convergence
    if (error < PREC) return iter+1;
  }

  return 0;
}


/*
 * Compute the exponential of a 3x3 matrix and its derivatives
 * using truncated series
 */
int expmat3_series(const double A[],double expA[],double dExpA[][9],double d2ExpA[][9][9],
                   bool computeFirst,bool computeSecond) {

  static const int ITMAX = 25;
  static const double PREC = 1.e-16;

  int i,j,k,l,m,n,ik,il,im,in,kl,mn;
  double X[9] = {1.e0, 0.e0, 0.e0, 0.e0, 1.e0, 0.e0, 0.e0, 0.e0, 1.e0};
  double dX[9][9],d2X[9][9][9],Y[9],dY[9][9],d2Y[9][9][9];

  /*
   * Initialization
   */
  std::memcpy(expA,X,9*sizeof(double));

  if (computeFirst || computeSecond) {
    std::memset(dX,0,81*sizeof(double));
    dX[0][0] = dX[1][1] = dX[2][2] = dX[3][3] = dX[4][4] = dX[5][5] 
      = dX[6][6] = dX[7][7] = dX[8][8] = 1.e0;
    std::memcpy(dExpA,dX,81*sizeof(double));
  }

  if (computeSecond) {
    std::memset(d2X,0,729*sizeof(double));
    for (i=0; i < 3; i++)
      for (j=0; j < 3; j++)
        for (k=0; k < 3; k++) {
          d2X[i*3+k][k*3+j][i*3+j] += 0.5e0;
          d2X[j*3+k][i*3+j][i*3+k] += 0.5e0;
        }
    std::memcpy(d2ExpA,d2X,729*sizeof(double));
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
      std::memcpy(d2Y,d2X,729*sizeof(double));

      for (k=0, kl=0; k < 3; k++)
        for (l=0; l < 3; l++, kl++) 
          for (m=0, mn=0; m < 3; m++)
            for (n=0; n < 3; n++, mn++) mulmat3(d2Y[kl][mn],A,d2X[kl][mn]);
      for (i=0, im=0; i < 3; i++) 
        for (m=0, mn=0; m < 3; m++, im++)
          for (n=0, in=i*3; n < 3; n++, mn++, in++)
            for (k=0, kl=0; k < 3; k++)
              for (l=0; l < 3; l++, kl++) d2X[kl][mn][in] += dX[kl][im];
      for (i=0, ik=0; i < 3; i++)
        for (k=0, kl=0; k < 3; k++, ik++)
          for (l=0, il=i*3; l < 3; l++, kl++, il++) 
            for (m=0, mn=0; m < 3; m++)
              for (n=0; n < 3; n++, mn++) d2X[kl][mn][il] += dX[mn][ik];
      mulvec(coef,d2X[0][0],d2X[0][0],729);
      if (computeSecond) addvec(d2ExpA[0][0],d2X[0][0],d2ExpA[0][0],729);
    
      norm = nrmvec1(d2X[0][0],729);
      error = (norm > error) ? norm:error;
    }

    // first derivative
    if ((computeFirst || computeSecond) && iter >= 1) {
      std::memcpy(dY,dX,81*sizeof(double));

      for (k=0, kl=0; k < 3; k++)
        for (l=0; l < 3; l++, kl++) mulmat3(dY[kl],A,dX[kl]);
      for (i=0, ik=0; i < 3; i++)
        for (k=0, kl=0; k < 3; k++, ik++)
          for (l=0, il=i*3; l < 3; l++, kl++, il++) dX[kl][il] += X[ik];   
      mulvec(coef,dX[0],dX[0],81);
      if (computeFirst) addvec(dExpA[0],dX[0],dExpA[0],81);

      norm = nrmvec1(dX[0],81);
      error = (norm > error) ? norm:error;
    }

    // exponential
    std::memcpy(Y,X,9*sizeof(double));

    mulmat3(Y,A,X);
    mulvec(coef,X,X,9);
    addvec(expA,X,expA,9);

    norm = nrmvec1(X,9);
    error = (norm > error) ? norm:error;

    // check convergence
    if (error < PREC) return iter+1;
  }

  return 0;
}


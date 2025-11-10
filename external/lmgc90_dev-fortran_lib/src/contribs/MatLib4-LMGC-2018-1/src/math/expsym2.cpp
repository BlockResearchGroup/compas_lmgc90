/*
 *  $Id: expsym2.cpp 124 2013-01-11 16:41:33Z lstainier $
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
#include "eigsym.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// local prototypes
static void exp3Spectrum1(double[],double[],double[][3]);
static void exp2Spectrum1(double[],double[],double[][2]);
static void exp3Spectrum2(double[],double[],double[][3][3]);
static void exp2Spectrum2(double[],double[],double[][2][2]);

static const double TOLERANCE = 1.e-08;
static const double PRECISION = 1.e-16;

/*
 * Compute the exponential of a 3x3 symmetric matrix and its derivatives
 * using eigenvalue expansion
 */
int expsym3_spectral(const double A[],double expA[],double dExpA[][6],double d2ExpA[][6][6],
                     bool computeFirst,bool computeSecond) {

  int i,j,k,l,m,n,p,q,r,ij,kl,mn;
  double eigVal[3],eigVec[3][3];
  double eigValExp[3];

  // compute eigenvalues and eigenvectors
  int test = eigsym3(A,eigVal,eigVec);
  if (!test) return 0;

  // compute exponential of eigenvalues
  eigValExp[0] = std::exp(eigVal[0]);
  eigValExp[1] = std::exp(eigVal[1]);
  eigValExp[2] = std::exp(eigVal[2]);

  // compute exponential of matrix
  std::memset(expA,0,6*sizeof(double));
  for (k=0; k < 3; k++) {
    for (i=0, ij=0; i < 3; i++) {
      double coef = eigValExp[k]*eigVec[k][i];
      for (j=0; j <= i; j++, ij++)
        expA[ij] += coef*eigVec[k][j];
    }
  }

  // compute first derivative
  if (computeFirst) {

    // compute function f(v1,v2)
    double f[3][3];
    exp3Spectrum1(eigVal,eigValExp,f);

    for (k=0, kl=0; k < 3; k++)
      for (l=0; l <= k; l++, kl++)
        for (i=0, ij=0; i < 3; i++)
          for (j=0; j <= i; j++, ij++) {
            dExpA[kl][ij] = 0.e0;
            for (m=0; m < 3; m++)
              for (n=0; n < 3; n++)
                dExpA[kl][ij] += 0.5*f[m][n]*eigVec[m][i]*eigVec[n][j]*
                  (eigVec[m][k]*eigVec[n][l]+eigVec[m][l]*eigVec[n][k]);
          }
  }

  // compute second derivative
  if (computeSecond) {

    // compute function F(v1,v2,v3)
    double F[3][3][3];
    exp3Spectrum2(eigVal,eigValExp,F);

    for (k=0, kl=0; k < 3; k++)
      for (l=0; l <= k; l++, kl++)
        for (m=0, mn=0; m < 3; m++)
          for (n=0; n <= m; n++, mn++)
            for (i=0, ij=0; i < 3; i++)
              for (j=0; j <= i; j++, ij++) {
                d2ExpA[kl][mn][ij] = 0.e0;
                for (p=0; p < 3; p++)
                  for (q=0; q < 3; q++)
                    for (r=0; r < 3; r++)
                      d2ExpA[kl][mn][ij] += 0.25*F[p][q][r]*
                        (eigVec[p][i]*eigVec[r][j]*
                          (eigVec[q][k]*eigVec[r][l]*eigVec[p][m]*eigVec[q][n]
                          +eigVec[q][l]*eigVec[r][k]*eigVec[p][m]*eigVec[q][n]
                          +eigVec[q][k]*eigVec[r][l]*eigVec[p][n]*eigVec[q][m]
                          +eigVec[q][l]*eigVec[r][k]*eigVec[p][n]*eigVec[q][m])
                        +eigVec[q][j]*eigVec[r][i]*
                          (eigVec[p][l]*eigVec[r][k]*eigVec[p][m]*eigVec[q][n]
                          +eigVec[p][k]*eigVec[r][l]*eigVec[p][m]*eigVec[q][n]
                          +eigVec[p][l]*eigVec[r][k]*eigVec[p][n]*eigVec[q][m]
                          +eigVec[p][k]*eigVec[r][l]*eigVec[p][n]*eigVec[q][m]));
              }
    
  }

  return 1;
}


/*
 * Compute the exponential of a 2x2 symmetric matrix and its derivatives
 * using eigenvalue expansion
 */
int expsym2_spectral(const double A[],double expA[],double dExpA[][3],double d2ExpA[][3][3],
                     bool computeFirst,bool computeSecond) {

  int i,j,k,l,m,n,p,q,r,ij,kl,mn;
  double eigVal[2],eigVec[2][2];
  double eigValExp[2];

  // compute eigenvalues and eigenvectors
  int test = eigsym2(A,eigVal,eigVec);
  if (!test) return 0;

  // compute exponential of eigenvalues
  eigValExp[0] = std::exp(eigVal[0]);
  eigValExp[1] = std::exp(eigVal[1]);

  // compute exponential of matrix
  std::memset(expA,0,3*sizeof(double));
  for (k=0; k < 2; k++) {
    for (i=0, ij=0; i < 2; i++) {
      double coef = eigValExp[k]*eigVec[k][i];
      for (j=0; j <= i; j++, ij++)
        expA[ij] += coef*eigVec[k][j];
    }
  }

  // compute first derivative
  if (computeFirst) {

    // compute function f(v1,v2)
    double f[2][2];
    exp2Spectrum1(eigVal,eigValExp,f);

    for (k=0, kl=0; k < 2; k++)
      for (l=0; l <= k; l++, kl++)
        for (i=0, ij=0; i < 2; i++)
          for (j=0; j <= i; j++, ij++) {
            dExpA[kl][ij] = 0.e0;
            for (m=0; m < 2; m++)
              for (n=0; n < 2; n++)
                dExpA[kl][ij] += 0.5*f[m][n]*eigVec[m][i]*eigVec[n][j]*
                  (eigVec[m][k]*eigVec[n][l]+eigVec[m][l]*eigVec[n][k]);
          }
  }

  // compute second derivative
  if (computeSecond) {

    // compute function F(v1,v2,v3)
    double F[2][2][2];
    exp2Spectrum2(eigVal,eigValExp,F);

    for (k=0, kl=0; k < 2; k++)
      for (l=0; l <= k; l++, kl++)
        for (m=0, mn=0; m < 2; m++)
          for (n=0; n <= m; n++, mn++)
            for (i=0, ij=0; i < 2; i++)
              for (j=0; j <= i; j++, ij++) {
                d2ExpA[kl][mn][ij] = 0.e0;
                for (p=0; p < 2; p++)
                  for (q=0; q < 2; q++)
                    for (r=0; r < 2; r++)
                      d2ExpA[kl][mn][ij] += 0.25*F[p][q][r]*
                        (eigVec[p][i]*eigVec[r][j]*
                          (eigVec[q][k]*eigVec[r][l]*eigVec[p][m]*eigVec[q][n]
                          +eigVec[q][l]*eigVec[r][k]*eigVec[p][m]*eigVec[q][n]
                          +eigVec[q][k]*eigVec[r][l]*eigVec[p][n]*eigVec[q][m]
                          +eigVec[q][l]*eigVec[r][k]*eigVec[p][n]*eigVec[q][m])
                        +eigVec[q][j]*eigVec[r][i]*
                          (eigVec[p][l]*eigVec[r][k]*eigVec[p][m]*eigVec[q][n]
                          +eigVec[p][k]*eigVec[r][l]*eigVec[p][m]*eigVec[q][n]
                          +eigVec[p][l]*eigVec[r][k]*eigVec[p][n]*eigVec[q][m]
                          +eigVec[p][k]*eigVec[r][l]*eigVec[p][n]*eigVec[q][m]));
              }
  }

  return 1;
}

void exp3Spectrum1(double eigVal[],double eigValExp[],double f[][3]) {
  
  double tol;
  double norm = std::fabs(eigVal[0])+std::fabs(eigVal[1])+std::fabs(eigVal[2]);
  if (norm > TOLERANCE) 
    tol = TOLERANCE*norm;
  else
    tol = PRECISION;
  
  f[0][0] = eigValExp[0];
  f[1][1] = eigValExp[1];
  f[2][2] = eigValExp[2];

  if (std::fabs(eigVal[0]-eigVal[1]) >= tol)
    f[0][1] = (eigValExp[1]-eigValExp[0])/(eigVal[1]-eigVal[0]);
  else 
    f[0][1] = eigValExp[0];
  f[1][0] = f[0][1];

  if (std::fabs(eigVal[0]-eigVal[2]) >= tol)
    f[0][2] = (eigValExp[2]-eigValExp[0])/(eigVal[2]-eigVal[0]);
  else 
    f[0][2] = eigValExp[0];
  f[2][0] = f[0][2];

  if (std::fabs(eigVal[1]-eigVal[2]) >= tol)
    f[1][2] = (eigValExp[2]-eigValExp[1])/(eigVal[2]-eigVal[1]);
  else 
    f[1][2] = eigValExp[1];
  f[2][1] = f[1][2];
}

void exp2Spectrum1(double eigVal[],double eigValExp[],double f[][2]) {
  
  double tol;
  double norm = std::fabs(eigVal[0])+std::fabs(eigVal[1]);
  if (norm > TOLERANCE) 
    tol = TOLERANCE*norm;
  else
    tol = PRECISION;
  
  f[0][0] = eigValExp[0];
  f[1][1] = eigValExp[1];

  if (std::fabs(eigVal[0]-eigVal[1]) >= tol)
    f[0][1] = (eigValExp[1]-eigValExp[0])/(eigVal[1]-eigVal[0]);
  else
    f[0][1] = eigValExp[0];
  f[1][0] = f[0][1];
}

void exp3Spectrum2(double eigVal[],double eigValExp[],double F[][3][3]) {
  
  double tol;
  double norm = std::fabs(eigVal[0])+std::fabs(eigVal[1])+std::fabs(eigVal[2]);
  if (norm > TOLERANCE) 
    tol = TOLERANCE*norm;
  else
    tol = PRECISION;

  int i,j,k;
  for (i=0; i < 3; i++)
    for (j=0; j < 3; j++)
      for (k=0; k < 3; k++) {
        
        bool test1=true,test2=true,test3=true;
        if (i != j) test1 = (std::fabs(eigVal[i]-eigVal[j]) < tol);
        if (i != k) test2 = (std::fabs(eigVal[i]-eigVal[k]) < tol);
        if (j != k) test3 = (std::fabs(eigVal[j]-eigVal[k]) < tol);
        
        if (!test1 && !test2 && !test3)
          F[i][j][k] = (eigVal[j]*eigValExp[i]-eigVal[k]*eigValExp[i]
                       -eigVal[i]*eigValExp[j]+eigVal[k]*eigValExp[j]
                       +eigVal[i]*eigValExp[k]-eigVal[j]*eigValExp[k])/
                       ((eigVal[i]-eigVal[j])*(eigVal[i]-eigVal[k])*(eigVal[j]-eigVal[k]));

        else if (test1 && !test2 && !test3)
          F[i][j][k] = (-eigValExp[i]+eigVal[i]*eigValExp[i]
                        -eigVal[k]*eigValExp[i]+eigValExp[k])/
                       ((eigVal[i]-eigVal[k])*(eigVal[i]-eigVal[k]));

        else if (!test1 && test2 && !test3)
          F[i][j][k] = (-eigValExp[k]+eigVal[k]*eigValExp[k]
                        -eigVal[j]*eigValExp[k]+eigValExp[j])/
                       ((eigVal[k]-eigVal[j])*(eigVal[k]-eigVal[j]));

        else if (!test1 && !test2 && test3)
          F[i][j][k] = (-eigValExp[j]+eigVal[j]*eigValExp[j]
                        -eigVal[i]*eigValExp[j]+eigValExp[i])/
                       ((eigVal[j]-eigVal[i])*(eigVal[j]-eigVal[i]));

        else
          F[i][j][k] = 0.5*eigValExp[i];
      }
}

void exp2Spectrum2(double eigVal[],double eigValExp[],double F[][2][2]) {
  
  double tol;
  double norm = std::fabs(eigVal[0])+std::fabs(eigVal[1]);
  if (norm > TOLERANCE) 
    tol = TOLERANCE*norm;
  else
    tol = PRECISION;

  int i,j,k;
  for (i=0; i < 2; i++)
    for (j=0; j < 2; j++)
      for (k=0; k < 2; k++) {
        
        bool test1=true,test2=true,test3=true;
        if (i != j) test1 = (std::fabs(eigVal[i]-eigVal[j]) < tol);
        if (i != k) test2 = (std::fabs(eigVal[i]-eigVal[k]) < tol);
        if (j != k) test3 = (std::fabs(eigVal[j]-eigVal[k]) < tol);
        
        if (!test1 && !test2 && !test3)
          F[i][j][k] = (eigVal[j]*eigValExp[i]-eigVal[k]*eigValExp[i]
                       -eigVal[i]*eigValExp[j]+eigVal[k]*eigValExp[j]
                       +eigVal[i]*eigValExp[k]-eigVal[j]*eigValExp[k])/
                       ((eigVal[i]-eigVal[j])*(eigVal[i]-eigVal[k])*(eigVal[j]-eigVal[k]));

        else if (test1 && !test2 && !test3)
          F[i][j][k] = (-eigValExp[i]+eigVal[i]*eigValExp[i]
                        -eigVal[k]*eigValExp[i]+eigValExp[k])/
                       ((eigVal[i]-eigVal[k])*(eigVal[i]-eigVal[k]));

        else if (!test1 && test2 && !test3)
          F[i][j][k] = (-eigValExp[k]+eigVal[k]*eigValExp[k]
                        -eigVal[j]*eigValExp[k]+eigValExp[j])/
                       ((eigVal[k]-eigVal[j])*(eigVal[k]-eigVal[j]));

        else if (!test1 && !test2 && test3)
          F[i][j][k] = (-eigValExp[j]+eigVal[j]*eigValExp[j]
                        -eigVal[i]*eigValExp[j]+eigValExp[i])/
                       ((eigVal[j]-eigVal[i])*(eigVal[j]-eigVal[i]));

        else
          F[i][j][k] = 0.5*eigValExp[i];
      }
}


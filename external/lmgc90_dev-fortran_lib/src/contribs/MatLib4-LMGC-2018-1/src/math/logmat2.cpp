/*
 *  $Id: logmat2.cpp 124 2013-01-11 16:41:33Z lstainier $
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
#include "eigmat.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// local prototypes
static void logSpectrum3(double[],double[],double[][3]);
static void logSpectrum2(double[],double[],double[][2]);
static void expSpectrum3(double[],double[],double[][3][3]);
static void expSpectrum2(double[],double[],double[][2][2]);

static const double TOLERANCE = 1.e-08;
static const double PRECISION = 1.e-16;

/*
 * Compute the logarithm of a 3x3 matrix and its derivatives
 * using eigenvalue expansion
 */
int logmat3_spectral(const double A[],double logA[],double dLogA[][9],double d2LogA[][9][9],
                     bool computeFirst,bool computeSecond) {

  int i,j,k,l,m,n,p,q,r,ij,kl,mn,pq;
  double eigVal[3],eigVecL[3][3],eigVecR[3][3];
  double eigValLog[3];

  // compute eigenvalues and eigenvectors
  int test = eigenLR3(A,eigVal,eigVecL,eigVecR);
  if (!test) return 0;

  // compute logarithm of eigenvalues
  eigValLog[0] = std::log(eigVal[0]);
  eigValLog[1] = std::log(eigVal[1]);
  eigValLog[2] = std::log(eigVal[2]);

  // compute logarithm of matrix
  std::memset(logA,0,9*sizeof(double));
  for (k=0; k < 3; k++) {
    for (i=0, ij=0; i < 3; i++) {
      double coef = eigValLog[k]*eigVecR[k][i];
      for (j=0; j < 3; j++, ij++)
        logA[ij] += coef*eigVecL[k][j];
    }
  }

  // compute first derivative
  if (computeFirst || computeSecond) {

    // compute function g(v1,v2)
    double g[3][3];
    logSpectrum3(eigVal,eigValLog,g);

    for (k=0, kl=0; k < 3; k++)
      for (l=0; l < 3; l++, kl++)
        for (i=0, ij=0; i < 3; i++)
          for (j=0; j < 3; j++, ij++) {
            dLogA[kl][ij] = 0.e0;
            for (m=0; m < 3; m++)
              for (n=0; n < 3; n++)
                dLogA[kl][ij] += 
                  g[m][n]*eigVecR[m][i]*eigVecL[n][j]*eigVecL[m][k]*eigVecR[n][l];
          }
  }

  // compute second derivative
  if (computeSecond) {

    // compute function G(v1,v2,v3)
    double G[3][3][3];
    double dum1[9][9][9],dum2[9][9][9];
    expSpectrum3(eigValLog,eigVal,G);

    for (k=0, kl=0; k < 3; k++)
      for (l=0; l < 3; l++, kl++)
        for (m=0, mn=0; m < 3; m++)
          for (n=0; n < 3; n++, mn++)
            for (i=0, ij=0; i < 3; i++)
              for (j=0; j < 3; j++, ij++) {
                dum1[kl][mn][ij] = 0.e0;
                for (p=0; p < 3; p++)
                  for (q=0; q < 3; q++)
                    for (r=0; r < 3; r++)
                      dum1[kl][mn][ij] -= 
                        G[p][q][r]*eigVecL[p][m]*eigVecR[q][n]*
                        (eigVecR[p][l]*eigVecL[q][j]*eigVecR[r][i]*eigVecL[r][k]
                        +eigVecR[p][i]*eigVecL[q][k]*eigVecR[r][l]*eigVecL[r][j]);
              }

    for (k=0, kl=0; k < 3; k++)
      for (l=0; l < 3; l++, kl++)
        for (m=0, mn=0; m < 3; m++)
          for (n=0; n < 3; n++, mn++)
            for (i=0, ij=0; i < 3; i++)
              for (j=0; j < 3; j++, ij++) {
                dum2[kl][mn][ij] = 0.e0;
                for (p=0, pq=0; p < 3; p++)
                  for (q=0; q < 3; q++, pq++)
                    dum2[kl][mn][ij] += dum1[kl][pq][ij]*dLogA[mn][pq];
              }

    for (k=0, kl=0; k < 3; k++)
      for (l=0; l < 3; l++, kl++)
        for (m=0, mn=0; m < 3; m++)
          for (n=0; n < 3; n++, mn++)
            for (i=0, ij=0; i < 3; i++)
              for (j=0; j < 3; j++, ij++) {
                dum1[kl][mn][ij] = 0.e0;
                for (p=0, pq=0; p < 3; p++)
                  for (q=0; q < 3; q++, pq++)
                    dum1[kl][mn][ij] += dum2[pq][mn][ij]*dLogA[kl][pq];
              }

    for (k=0, kl=0; k < 3; k++)
      for (l=0; l < 3; l++, kl++)
        for (m=0, mn=0; m < 3; m++)
          for (n=0; n < 3; n++, mn++)
            for (i=0, ij=0; i < 3; i++)
              for (j=0; j < 3; j++, ij++) {
                d2LogA[kl][mn][ij] = 0.e0;
                for (p=0, pq=0; p < 3; p++)
                  for (q=0; q < 3; q++, pq++)
                    d2LogA[kl][mn][ij] += dLogA[pq][ij]*dum1[kl][mn][pq];
              }
    
  }

  return 1;
}

/*
 * Compute the logarithm of a 2x2 matrix and its derivatives
 * using eigenvalue expansion
 */
int logmat2_spectral(const double A[],double logA[],double dLogA[][4],double d2LogA[][4][4],
                     bool computeFirst,bool computeSecond) {

  int i,j,k,l,m,n,p,q,r,ij,kl,mn,pq;
  double eigVal[2],eigVecL[2][2],eigVecR[2][2];
  double eigValLog[2];

  // compute eigenvalues and eigenvectors
  int test = eigenLR2(A,eigVal,eigVecL,eigVecR);
  if (!test) return 0;

  // compute logarithm of eigenvalues
  eigValLog[0] = std::log(eigVal[0]);
  eigValLog[1] = std::log(eigVal[1]);

  // compute logarithm of matrix
  std::memset(logA,0,4*sizeof(double));
  for (k=0; k < 2; k++) {
    for (i=0, ij=0; i < 2; i++) {
      double coef = eigValLog[k]*eigVecR[k][i];
      for (j=0; j < 2; j++, ij++)
        logA[ij] += coef*eigVecL[k][j];
    }
  }

  // compute first derivative
  if (computeFirst || computeSecond) {

    // compute function g(v1,v2)
    double g[2][2];
    logSpectrum2(eigVal,eigValLog,g);

    for (k=0, kl=0; k < 2; k++)
      for (l=0; l < 2; l++, kl++)
        for (i=0, ij=0; i < 2; i++)
          for (j=0; j < 2; j++, ij++) {
            dLogA[kl][ij] = 0.e0;
            for (m=0; m < 2; m++)
              for (n=0; n < 2; n++)
                dLogA[kl][ij] +=
                  g[m][n]*eigVecR[m][i]*eigVecL[n][j]*eigVecL[m][k]*eigVecR[n][l];
          }
  }

  // compute second derivative
  if (computeSecond) {

    // compute function G(v1,v2,v3)
    double G[2][2][2];
    double dum1[4][4][4],dum2[4][4][4];
    expSpectrum2(eigValLog,eigVal,G);

    for (k=0, kl=0; k < 2; k++)
      for (l=0; l < 2; l++, kl++)
        for (m=0, mn=0; m < 2; m++)
          for (n=0; n < 2; n++, mn++)
            for (i=0, ij=0; i < 2; i++)
              for (j=0; j < 2; j++, ij++) {
                dum1[kl][mn][ij] = 0.e0;
                for (p=0; p < 2; p++)
                  for (q=0; q < 2; q++)
                    for (r=0; r < 2; r++)
                      dum1[kl][mn][ij] -=
                        G[p][q][r]*eigVecL[p][m]*eigVecR[q][n]*
                          (eigVecR[p][l]*eigVecL[q][j]*eigVecR[r][i]*eigVecL[r][k]
                          +eigVecR[p][i]*eigVecL[q][k]*eigVecR[r][l]*eigVecL[r][j]);
              }

    for (k=0, kl=0; k < 2; k++)
      for (l=0; l < 2; l++, kl++)
        for (m=0, mn=0; m < 2; m++)
          for (n=0; n < 2; n++, mn++)
            for (i=0, ij=0; i < 2; i++)
              for (j=0; j < 2; j++, ij++) {
                dum2[kl][mn][ij] = 0.e0;
                for (p=0, pq=0; p < 2; p++)
                  for (q=0; q < 2; q++, pq++)
                    dum2[kl][mn][ij] += dum1[kl][pq][ij]*dLogA[mn][pq];
              }

    for (k=0, kl=0; k < 2; k++)
      for (l=0; l < 2; l++, kl++)
        for (m=0, mn=0; m < 2; m++)
          for (n=0; n < 2; n++, mn++)
            for (i=0, ij=0; i < 2; i++)
              for (j=0; j < 2; j++, ij++) {
                dum1[kl][mn][ij] = 0.e0;
                for (p=0, pq=0; p < 2; p++)
                  for (q=0; q < 2; q++, pq++)
                    dum1[kl][mn][ij] += dum2[pq][mn][ij]*dLogA[kl][pq];
              }

    for (k=0, kl=0; k < 2; k++)
      for (l=0; l < 2; l++, kl++)
        for (m=0, mn=0; m < 2; m++)
          for (n=0; n < 2; n++, mn++)
            for (i=0, ij=0; i < 2; i++)
              for (j=0; j < 2; j++, ij++) {
                d2LogA[kl][mn][ij] = 0.e0;
                for (p=0, pq=0; p < 2; p++)
                  for (q=0; q < 2; q++, pq++)
                    d2LogA[kl][mn][ij] += dLogA[pq][ij]*dum1[kl][mn][pq];
              }
  }

  return 1;
}

void logSpectrum3(double eigVal[],double eigValLog[],double g[][3]) {
  
  double tol;
  double norm = std::fabs(eigVal[0])+std::fabs(eigVal[1])+std::fabs(eigVal[2]);
  if (norm > TOLERANCE) 
    tol = TOLERANCE*norm;
  else
    tol = PRECISION;
  
  g[0][0] = 1.e0/eigVal[0];
  g[1][1] = 1.e0/eigVal[1];
  g[2][2] = 1.e0/eigVal[2];

  if (std::fabs(eigVal[0]-eigVal[1]) >= tol)
    g[0][1] = (eigValLog[1]-eigValLog[0])/(eigVal[1]-eigVal[0]);
  else 
    g[0][1] = g[0][0];
  g[1][0] = g[0][1];

  if (std::fabs(eigVal[0]-eigVal[2]) >= tol)
    g[0][2] = (eigValLog[2]-eigValLog[0])/(eigVal[2]-eigVal[0]);
  else 
    g[0][2] = g[0][0];
  g[2][0] = g[0][2];

  if (std::fabs(eigVal[1]-eigVal[2]) >= tol)
    g[1][2] = (eigValLog[2]-eigValLog[1])/(eigVal[2]-eigVal[1]);
  else 
    g[1][2] = g[1][1];
  g[2][1] = g[1][2];
}

void logSpectrum2(double eigVal[],double eigValLog[],double g[][2]) {
  
  double tol;
  double norm = std::fabs(eigVal[0])+std::fabs(eigVal[1]);
  if (norm > TOLERANCE) 
    tol = TOLERANCE*norm;
  else
    tol = PRECISION;
  
  g[0][0] = 1.e0/eigVal[0];
  g[1][1] = 1.e0/eigVal[1];

  if (std::fabs(eigVal[0]-eigVal[1]) >= tol)
    g[0][1] = (eigValLog[1]-eigValLog[0])/(eigVal[1]-eigVal[0]);
  else
    g[0][1] = g[0][0];
  g[1][0] = g[0][1];
}

void expSpectrum3(double eigVal[],double eigValExp[],double G[][3][3]) {
  
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
          G[i][j][k] = (eigVal[j]*eigValExp[i]-eigVal[k]*eigValExp[i]
                       -eigVal[i]*eigValExp[j]+eigVal[k]*eigValExp[j]
                       +eigVal[i]*eigValExp[k]-eigVal[j]*eigValExp[k])/
                       ((eigVal[i]-eigVal[j])*(eigVal[i]-eigVal[k])*(eigVal[j]-eigVal[k]));

        else if (test1 && !test2 && !test3)
          G[i][j][k] = (-eigValExp[i]+eigVal[i]*eigValExp[i]
                        -eigVal[k]*eigValExp[i]+eigValExp[k])/
                       ((eigVal[i]-eigVal[k])*(eigVal[i]-eigVal[k]));

        else if (!test1 && test2 && !test3)
          G[i][j][k] = (-eigValExp[k]+eigVal[k]*eigValExp[k]
                        -eigVal[j]*eigValExp[k]+eigValExp[j])/
                       ((eigVal[k]-eigVal[j])*(eigVal[k]-eigVal[j]));

        else if (!test1 && !test2 && test3)
          G[i][j][k] = (-eigValExp[j]+eigVal[j]*eigValExp[j]
                        -eigVal[i]*eigValExp[j]+eigValExp[i])/
                       ((eigVal[j]-eigVal[i])*(eigVal[j]-eigVal[i]));

        else
          G[i][j][k] = 0.5*eigValExp[i];
      }
}

void expSpectrum2(double eigVal[],double eigValExp[],double G[][2][2]) {
  
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
          G[i][j][k] = (eigVal[j]*eigValExp[i]-eigVal[k]*eigValExp[i]
                       -eigVal[i]*eigValExp[j]+eigVal[k]*eigValExp[j]
                       +eigVal[i]*eigValExp[k]-eigVal[j]*eigValExp[k])/
                       ((eigVal[i]-eigVal[j])*(eigVal[i]-eigVal[k])*(eigVal[j]-eigVal[k]));

        else if (test1 && !test2 && !test3)
          G[i][j][k] = (-eigValExp[i]+eigVal[i]*eigValExp[i]
                        -eigVal[k]*eigValExp[i]+eigValExp[k])/
                       ((eigVal[i]-eigVal[k])*(eigVal[i]-eigVal[k]));

        else if (!test1 && test2 && !test3)
          G[i][j][k] = (-eigValExp[k]+eigVal[k]*eigValExp[k]
                        -eigVal[j]*eigValExp[k]+eigValExp[j])/
                       ((eigVal[k]-eigVal[j])*(eigVal[k]-eigVal[j]));

        else if (!test1 && !test2 && test3)
          G[i][j][k] = (-eigValExp[j]+eigVal[j]*eigValExp[j]
                        -eigVal[i]*eigValExp[j]+eigValExp[i])/
                       ((eigVal[j]-eigVal[i])*(eigVal[j]-eigVal[i]));

        else
          G[i][j][k] = 0.5*eigValExp[i];
      }
}


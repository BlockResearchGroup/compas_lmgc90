/*
 *  $Id: Tensor2D.cpp 133 2013-07-22 18:47:44Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "Tensor2D.h"

// std C library
#include <cmath>
// local
#include <math/SymTensor2D.h>
#include <math/Vector3D.h>
extern int expmat2_series(const double[],double[],double[][4],double[][4][4],bool,bool);
extern int expmat2_spectral(const double[],double[],double[][4],double[][4][4],bool,bool);
extern int expmat2_linear(const double[],double[],double[][4],double[][4][4],bool,bool);
extern int logmat2_series(const double[],double[],double[][4],double[][4][4],bool,bool);
extern int logmat2_spectral(const double[],double[],double[][4],double[][4][4],bool,bool);
extern int logmat2_linear(const double[],double[],double[][4],double[][4][4],bool,bool);

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for Tensor2D.
 */

// index map
const int Tensor2D::MAP[3][3] = {{ 0, 1,-1},{ 2, 3,-1},{-1,-1, 4}};

// specific arithmetic operators
Tensor2D Tensor2D::operator*(const Tensor2D& A) const {
  Tensor2D B;
  B[0] = (*this)[0]*A[0]+(*this)[1]*A[2];
  B[1] = (*this)[0]*A[1]+(*this)[1]*A[3];
  B[2] = (*this)[2]*A[0]+(*this)[3]*A[2];
  B[3] = (*this)[2]*A[1]+(*this)[3]*A[3];
  B[4] = (*this)[4]*A[4];
  return B;
}
Tensor2D Tensor2D::operator*(const SymTensor2D& A) const {
  Tensor2D B;
  B[0] = (*this)[0]*A[0]+(*this)[1]*A[1];
  B[1] = (*this)[0]*A[1]+(*this)[1]*A[2];
  B[2] = (*this)[2]*A[0]+(*this)[3]*A[1];
  B[3] = (*this)[2]*A[1]+(*this)[3]*A[2];
  B[4] = (*this)[4]*A[3];
  return B;
}
Vector3D Tensor2D::operator*(const Vector3D& a) const {
  Vector3D b;
  b[0] = (*this)[0]*a[0]+(*this)[1]*a[1];
  b[1] = (*this)[2]*a[0]+(*this)[3]*a[1];
  b[2] = (*this)[4]*a[2];
  return b;
}

// symmetrize (use this operation on strains)
SymTensor2D Tensor2D::covariantSym() const {
  SymTensor2D S;
  S[0] = (*this)[0];
  S[1] = (*this)[1]+(*this)[2];
  S[2] = (*this)[3];
  S[3] = (*this)[4];
  return S;
}

// symmetrize (use this operation on stresses)
SymTensor2D Tensor2D::contravariantSym() const {
  SymTensor2D S;
  S[0] = (*this)[0];
  S[1] = 0.5*((*this)[1]+(*this)[2]);
  S[2] = (*this)[3];
  S[3] = (*this)[4];
  return S;
}

// compute determinant
double Tensor2D::determinant() const {
  return ((*this)[0]*(*this)[3]-(*this)[1]*(*this)[2])*(*this)[4];
}

// compute trace
double Tensor2D::trace() const {
  return (*this)[0]+(*this)[3]+(*this)[4];
}

// compute inverse
Tensor2D Tensor2D::inverse(double& det) const {
  Tensor2D A;
  det = this->determinant();
  double detInv = (*this)[4]/det;
  A[0] =  (*this)[3]*detInv;
  A[1] = -(*this)[1]*detInv;
  A[2] = -(*this)[2]*detInv;
  A[3] =  (*this)[0]*detInv;
  A[4] = 1.e0/(*this)[4];
  return A;
}

// compute transposed
Tensor2D Tensor2D::transposed() const {
  Tensor2D A;
  A[0] = (*this)[0];
  A[1] = (*this)[2];
  A[2] = (*this)[1];
  A[3] = (*this)[3];
  A[4] = (*this)[4];
  return A;
}

// tensor exponential
Tensor2D Tensor2D::exp(Tensor2D dExp[],Tensor2D d2Exp[][5],
                       bool first,bool second) const {
  unsigned int i,j,k;
  double vA[4],vExpA[4],dExpA[4][4],d2ExpA[4][4][4];
  Tensor2D expA;
  
  for (i=0; i < 4; i++) vA[i] = (*this)[i];
  double norm = vA[0]*(*this)[0]+vA[1]*vA[1]+vA[2]*(*this)[2]+vA[3]*vA[3];
  int test;
  if (norm < 2.25) {
    test = expmat2_series(vA,vExpA,dExpA,d2ExpA,first,second);
    if (!test) // OUCH!
      test = expmat2_spectral(vA,vExpA,dExpA,d2ExpA,first,second);
  }
  else
    test = expmat2_spectral(vA,vExpA,dExpA,d2ExpA,first,second);
  if (!test) // OUCH! OUCH! OUCH!
    test = expmat2_linear(vA,vExpA,dExpA,d2ExpA,first,second);
  if (!test) throw ZException("Tensor2D::exp()");

  for (i=0; i < 4; i++) expA[i] = vExpA[i];
  expA[4] = std::exp((*this)[4]);
  if (first) {
    for (i=0; i < 4; i++)
      for (j=0; j < 4; j++) dExp[i][j] = dExpA[i][j];
    dExp[0][4] = dExp[1][4] = dExp[2][4] = dExp[3][4] = 0.0e0;
    dExp[4][0] = dExp[4][1] = dExp[4][2] = dExp[4][3] = 0.0e0;
    dExp[4][4] = expA[4];
  }
  if (second) {
    for (i=0; i < 4; i++)
      for (j=0; j < 4; j++)
        for (k=0; k < 4; k++) d2Exp[i][j][k] = d2ExpA[i][j][k];
    d2Exp[0][0][4] = d2Exp[0][1][4] = d2Exp[0][2][4] = d2Exp[0][3][4] = 0.0e0;
    d2Exp[1][0][4] = d2Exp[1][1][4] = d2Exp[1][2][4] = d2Exp[1][3][4] = 0.0e0;
    d2Exp[2][0][4] = d2Exp[2][1][4] = d2Exp[2][2][4] = d2Exp[2][3][4] = 0.0e0;
    d2Exp[3][0][4] = d2Exp[3][1][4] = d2Exp[3][2][4] = d2Exp[3][3][4] = 0.0e0;
    d2Exp[0][4][0] = d2Exp[1][4][0] = d2Exp[2][4][0] = d2Exp[3][4][0] = 0.0e0;
    d2Exp[0][4][1] = d2Exp[1][4][1] = d2Exp[2][4][1] = d2Exp[3][4][1] = 0.0e0;
    d2Exp[0][4][2] = d2Exp[1][4][2] = d2Exp[2][4][2] = d2Exp[3][4][2] = 0.0e0;
    d2Exp[0][4][3] = d2Exp[1][4][3] = d2Exp[2][4][3] = d2Exp[3][4][3] = 0.0e0;
    d2Exp[4][0][0] = d2Exp[4][1][0] = d2Exp[4][2][0] = d2Exp[4][3][0] = 0.0e0;
    d2Exp[4][0][1] = d2Exp[4][1][1] = d2Exp[4][2][1] = d2Exp[4][3][1] = 0.0e0;
    d2Exp[4][0][2] = d2Exp[4][1][2] = d2Exp[4][2][2] = d2Exp[4][3][2] = 0.0e0;
    d2Exp[4][0][3] = d2Exp[4][1][3] = d2Exp[4][2][3] = d2Exp[4][3][3] = 0.0e0;
    d2Exp[0][4][4] = d2Exp[1][4][4] = d2Exp[2][4][4] = d2Exp[3][4][4] = 0.0e0;
    d2Exp[4][0][4] = d2Exp[4][1][4] = d2Exp[4][2][4] = d2Exp[4][3][4] = 0.0e0;
    d2Exp[4][4][0] = d2Exp[4][4][1] = d2Exp[4][4][2] = d2Exp[4][4][3] = 0.0e0;
    d2Exp[4][4][4] = expA[4];
  }
  
  return expA;
}

// tensor logarithm
Tensor2D Tensor2D::log(Tensor2D dLog[],Tensor2D d2Log[][5],
                       bool first,bool second) const {
  unsigned int i,j,k;
  double vA[4],vLogA[4],dLogA[4][4],d2LogA[4][4][4];
  Tensor2D logA;
  
  for (i=0; i < 4; i++) vA[i] = (*this)[i];
  double norm = (vA[0]-1.0)*(vA[0]-1.0)+vA[1]*vA[1]
                +vA[2]*vA[2]+(vA[3]-1.0)*(vA[3]-1.0);
  int test;
  if (norm < 0.04) {
    test = logmat2_series(vA,vLogA,dLogA,d2LogA,first,second);
    if (!test) // OUCH!
      test = logmat2_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  }
  else
    test = logmat2_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  if (!test) // OUCH! OUCH! OUCH!
    test = logmat2_linear(vA,vLogA,dLogA,d2LogA,first,second);
  if (!test) throw ZException("Tensor2D::log()");

  for (i=0; i < 4; i++) logA[i] = vLogA[i];
  logA[4] = std::log((*this)[4]);
  if (first) {
    for (i=0; i < 4; i++)
      for (j=0; j < 4; j++) dLog[i][j] = dLogA[i][j];
    dLog[0][4] = dLog[1][4] = dLog[2][4] = dLog[3][4] = 0.0e0;
    dLog[4][0] = dLog[4][1] = dLog[4][2] = dLog[4][3] = 0.0e0;
    dLog[4][4] = 1.0e0/(*this)[4];
  }
  if (second) {
    for (i=0; i < 4; i++)
      for (j=0; j < 4; j++)
        for (k=0; k < 4; k++) d2Log[i][j][k] = d2LogA[i][j][k];
    d2Log[0][0][4] = d2Log[0][1][4] = d2Log[0][2][4] = d2Log[0][3][4] = 0.0e0;
    d2Log[1][0][4] = d2Log[1][1][4] = d2Log[1][2][4] = d2Log[1][3][4] = 0.0e0;
    d2Log[2][0][4] = d2Log[2][1][4] = d2Log[2][2][4] = d2Log[2][3][4] = 0.0e0;
    d2Log[3][0][4] = d2Log[3][1][4] = d2Log[3][2][4] = d2Log[3][3][4] = 0.0e0;
    d2Log[0][4][0] = d2Log[1][4][0] = d2Log[2][4][0] = d2Log[3][4][0] = 0.0e0;
    d2Log[0][4][1] = d2Log[1][4][1] = d2Log[2][4][1] = d2Log[3][4][1] = 0.0e0;
    d2Log[0][4][2] = d2Log[1][4][2] = d2Log[2][4][2] = d2Log[3][4][2] = 0.0e0;
    d2Log[0][4][3] = d2Log[1][4][3] = d2Log[2][4][3] = d2Log[3][4][3] = 0.0e0;
    d2Log[4][0][0] = d2Log[4][1][0] = d2Log[4][2][0] = d2Log[4][3][0] = 0.0e0;
    d2Log[4][0][1] = d2Log[4][1][1] = d2Log[4][2][1] = d2Log[4][3][1] = 0.0e0;
    d2Log[4][0][2] = d2Log[4][1][2] = d2Log[4][2][2] = d2Log[4][3][2] = 0.0e0;
    d2Log[4][0][3] = d2Log[4][1][3] = d2Log[4][2][3] = d2Log[4][3][3] = 0.0e0;
    d2Log[0][4][4] = d2Log[1][4][4] = d2Log[2][4][4] = d2Log[3][4][4] = 0.0e0;
    d2Log[4][0][4] = d2Log[4][1][4] = d2Log[4][2][4] = d2Log[4][3][4] = 0.0e0;
    d2Log[4][4][0] = d2Log[4][4][1] = d2Log[4][4][2] = d2Log[4][4][3] = 0.0e0;
    d2Log[4][4][4] = -dLog[4][4]*dLog[4][4];
  }
  
  return logA;
}

// identity tensor
Tensor2D Tensor2D::identity() {
  Tensor2D I;
  I[0] = I[3] = I[4] = 1.0e0;
  I[1] = I[2] = 0.0e0;
  return I;
}

// export as square matrix
ShortSqrMatrix Tensor2D::toMatrix() const {
  ShortSqrMatrix M(3);
  M = 0.e0;
  M[0][0] = (*this)[0];
  M[0][1] = (*this)[1];
  M[1][0] = (*this)[2];
  M[1][1] = (*this)[3];
  M[2][2] = (*this)[4];
  return M;
}


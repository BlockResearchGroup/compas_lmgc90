/*
 *  $Id: SymTensor2D.cpp 130 2013-04-11 01:18:02Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "SymTensor2D.h"

// std C library
#include <cmath>
// local
#include <math/Tensor2D.h>
#include <math/Vector2D.h>
#include <math/Vector3D.h>
#include "eigsym.h"
extern int expsym2_series(const double[],double[],double[][3],double[][3][3],bool,bool);
extern int expsym2_spectral(const double[],double[],double[][3],double[][3][3],bool,bool);
extern int expsym2_linear(const double[],double[],double[][3],double[][3][3],bool,bool);
extern int logsym2_series(const double[],double[],double[][3],double[][3][3],bool,bool);
extern int logsym2_spectral(const double[],double[],double[][3],double[][3][3],bool,bool);
extern int logsym2_linear(const double[],double[],double[][3],double[][3][3],bool,bool);

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for StdSymTensor2D.
 */

// index map
const int StdSymTensor2D::MAP[2][2] = {{ 0, 1},{ 1, 2}};

// compute determinant
double StdSymTensor2D::determinant() const {
  return (*this)[0]*(*this)[2]-(*this)[1]*(*this)[1];
}

// compute eigenvalues
void StdSymTensor2D::eigenValues(double v[]) const {
  
  // compute invariants
  double I1 = this->trace();
  double I3 = this->determinant();
  
  // compute eigenvalues
  double val = std::sqrt(I1*I1-4*I3);
  v[0] = 0.5*(I1-val);
  v[1] = 0.5*(I1+val);
}

// compute eigenvalues and eigenbases
void StdSymTensor2D::eigenSplit(double v[],StdSymTensor2D M[]) const {

  double vA[3],eigVec[2][2];
  for (unsigned int ij=0; ij < 3; ij++) vA[ij] = (*this)[ij];
  int test = eigsym2(vA,v,eigVec);
  if (!test) throw ZException("StdSymTensor2D::eigenSplit()");
  
  for (unsigned int k=0; k < 2; k++) {
    M[k][0] = eigVec[k][0]*eigVec[k][0];
    M[k][1] = eigVec[k][0]*eigVec[k][1];
    M[k][2] = eigVec[k][1]*eigVec[k][1];
  }
}

// compute trace
double StdSymTensor2D::trace() const {
  return (*this)[0]+(*this)[2];
}

// transform to covariant
StdSymTensor2D StdSymTensor2D::covariant() const {
  StdSymTensor2D A(*this);
  A[1] *= 2.0;
  return A;
}

// transform to contravariant
StdSymTensor2D StdSymTensor2D::contravariant() const {
  StdSymTensor2D A(*this);
  A[1] *= 0.5;
  return A;
}

// compute inverse
StdSymTensor2D StdSymTensor2D::inverse(double& det) const {
  StdSymTensor2D A;
  det = this->determinant();
  double detInv = 1.e0/det;
  A[0] =  (*this)[2]*detInv;
  A[1] = -(*this)[1]*detInv;
  A[2] =  (*this)[0]*detInv;
  return A;
}

// compute exponential
StdSymTensor2D StdSymTensor2D::exp(StdSymTensor2D dExp[],StdSymTensor2D d2Exp[][3],
                                   bool first,bool second) const {
  unsigned int i,j,k;
  double vA[3],vExpA[3],dExpA[3][3],d2ExpA[3][3][3];
  StdSymTensor2D expA;
  
  for (i=0; i < 3; i++) vA[i] = (*this)[i];
  double norm = vA[0]*vA[0]+vA[1]*vA[1]+vA[2]*vA[2];
  int test;
  if (norm < 2.25) {
    test = expsym2_series(vA,vExpA,dExpA,d2ExpA,first,second);
    if (!test)
      test = expsym2_spectral(vA,vExpA,dExpA,d2ExpA,first,second);
  }
  else
    test = expsym2_spectral(vA,vExpA,dExpA,d2ExpA,first,second);
  if (!test)
    test = expsym2_linear(vA,vExpA,dExpA,d2ExpA,first,second);
  if (!test) throw ZException("StdSymTensor2D::exp()");

  for (i=0; i < 3; i++) expA[i] = vExpA[i];
  if (first) {
    for (i=0; i < 3; i++)
      for (j=0; j < 3; j++) dExp[i][j] = dExpA[i][j];
  }
  if (second) {
    for (i=0; i < 3; i++)
      for (j=0; j < 3; j++)
        for (k=0; k < 3; k++) d2Exp[i][j][k] = d2ExpA[i][j][k];
  }
  
  return expA;
}

// compute logarithm
StdSymTensor2D StdSymTensor2D::log(StdSymTensor2D dLog[],
                                   StdSymTensor2D d2Log[][3],
                                   bool first,bool second) const {
  unsigned int i,j,k;
  double vA[3],vLogA[3],dLogA[3][3],d2LogA[3][3][3];
  StdSymTensor2D logA;
  
  for (i=0; i < 3; i++) vA[i] = (*this)[i];
  double norm = (vA[0]-1.0)*(vA[0]-1.0)+vA[1]*vA[1]+(vA[2]-1.0)*(vA[2]-1.0);
  int test;
  if (norm < 2.25) {
    test = logsym2_series(vA,vLogA,dLogA,d2LogA,first,second);
    if (!test)
      test = logsym2_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  }
  else
    test = logsym2_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  if (!test)
    test = logsym2_linear(vA,vLogA,dLogA,d2LogA,first,second);
  if (!test) throw ZException("StdSymTensor2D::log()");

  for (i=0; i < 3; i++) logA[i] = vLogA[i];
  if (first) {
    for (i=0; i < 3; i++)
      for (j=0; j < 3; j++) dLog[i][j] = dLogA[i][j];
  }
  if (second) {
    for (i=0; i < 3; i++)
      for (j=0; j < 3; j++)
        for (k=0; k < 3; k++) d2Log[i][j][k] = d2LogA[i][j][k];
  }
  
  return logA;
}

// identity tensor
StdSymTensor2D StdSymTensor2D::identity() {
  StdSymTensor2D I;
  I[0] = I[2] = 1.0e0;
  I[1] = 0.0e0;
  return I;
}
// build tensor by vector outer product
StdSymTensor2D StdSymTensor2D::outerProd(const Vector2D& v) {
  SymTensor2D A;
  A[0] = v[0]*v[0];
  A[1] = v[0]*v[1];
  A[2] = v[1]*v[1];
  return A;
}
StdSymTensor2D StdSymTensor2D::outerProd(const Vector2D& a,const Vector2D& b) {
  SymTensor2D C;
  C[0] = a[0]*b[0];
  C[1] = 0.5*(a[0]*b[1]+a[1]*b[0]);
  C[2] = a[1]*b[1];
  return C;
}

// export as square matrix
ShortSqrMatrix StdSymTensor2D::toMatrix() const {
  ShortSqrMatrix M(2);
  M[0][0] = (*this)[0];
  M[0][1] = M[1][0] = (*this)[1];
  M[1][1] = (*this)[2];
  return M;
}

// full inner product
double MATLIB_NAMESPACE innerProd2(const StdSymTensor2D& A,const StdSymTensor2D& B) {
  return A[0]*B[0]+A[2]*B[2]+(A[1]*B[1])*2;
}


/*
 * Methods for SymTensor2D.
 */

// index map
const int SymTensor2D::MAP[3][3] = {{ 0, 1,-1},{ 1, 2,-1},{-1,-1, 3}};

// specific arithmetic operators
Tensor2D SymTensor2D::operator*(const Tensor2D& A) const {
  Tensor2D B;
  B[0] = (*this)[0]*A[0]+(*this)[1]*A[2];
  B[1] = (*this)[0]*A[1]+(*this)[1]*A[3];
  B[2] = (*this)[1]*A[0]+(*this)[2]*A[2];
  B[3] = (*this)[1]*A[1]+(*this)[2]*A[3];
  B[4] = (*this)[3]*A[4];
  return B;
}
Vector2D SymTensor2D::operator*(const Vector2D& a) const {
  Vector2D b;
  b[0] = (*this)[0]*a[0]+(*this)[1]*a[1];
  b[1] = (*this)[1]*a[0]+(*this)[2]*a[1];
  return b;
}
Vector3D SymTensor2D::operator*(const Vector3D& a) const {
  Vector3D b;
  b[0] = (*this)[0]*a[0]+(*this)[1]*a[1];
  b[1] = (*this)[1]*a[0]+(*this)[2]*a[1];
  b[2] = (*this)[3]*a[2];
  return b;
}

// compute determinant
double SymTensor2D::determinant() const {
  return ((*this)[0]*(*this)[2]-(*this)[1]*(*this)[1])*(*this)[3];
}

// compute eigenvalues
void SymTensor2D::eigenValues(double v[]) const {
  
  // compute invariants
  double I1 = (*this)[0]+(*this)[2];
  double I3 = (*this)[0]*(*this)[2]-(*this)[1]*(*this)[1];
  
  // compute eigenvalues
  double val = std::sqrt(I1*I1-4*I3);
  v[0] = 0.5*(I1-val);
  v[1] = 0.5*(I1+val);
  v[2] = (*this)[3];
}

// compute eigenvalues and eigenbases
void SymTensor2D::eigenSplit(double v[],SymTensor2D M[]) const {
  
  double vA[3],eigVec[2][2];
  for (unsigned int ij=0; ij < 3; ij++) vA[ij] = (*this)[ij];
  int test = eigsym2(vA,v,eigVec);
  if (!test) throw ZException("SymTensor2D::eigenSplit()");
  v[2] = (*this)[3];
  
  for (unsigned int k=0; k < 2; k++) {
    M[k][0] = eigVec[k][0]*eigVec[k][0];
    M[k][1] = eigVec[k][0]*eigVec[k][1];
    M[k][2] = eigVec[k][1]*eigVec[k][1];
    M[k][3] = 0.0e0;
  }
  M[2][0] = M[2][1] = M[2][2] = 0.0e0;
  M[2][3] = 1.0e0;
}

// compute trace
double SymTensor2D::trace() const {
  return (*this)[0]+(*this)[2]+(*this)[3];
}

// transform to covariant
SymTensor2D SymTensor2D::covariant() const {
  SymTensor2D A(*this);
  A[1] *= 2.0;
  return A;
}

// transform to contravariant
SymTensor2D SymTensor2D::contravariant() const {
  SymTensor2D A(*this);
  A[1] *= 0.5;
  return A;
}

// push-pull operations
SymTensor2D SymTensor2D::covariantPush(const Tensor2D& F) const {
  // g = F^-T*C*F^-1
  double dummy;
  Tensor2D FInv = F.inverse(dummy);
  return covariantPull(FInv);
}
SymTensor2D SymTensor2D::covariantPull(const Tensor2D& F) const {
  // C = F^T*g*F
  SymTensor2D C;
  unsigned int i,j,k,l;
  for (i=0; i < 2; i++)
    for (j=0; j < 2; j++) {
      double val = 0.0e0;
      for (k=0; k < 2; k++)
        for (l=0; l < 2; l++)
          val += F[Tensor2D::MAP[k][i]]*(*this)[MAP[k][l]]*F[Tensor2D::MAP[l][j]];
      C[MAP[i][j]] = val;
    }
  C[3] = F[4]*(*this)[3]*F[4];
  return C;
}
SymTensor2D SymTensor2D::contravariantPush(const Tensor2D& F) const {
  // t = F*S*F^T
  SymTensor2D tau;
  unsigned int i,j,k,l;
  for (i=0; i < 2; i++)
    for (j=0; j < 2; j++) {
      double val = 0.0e0;
      for (k=0; k < 2; k++)
        for (l=0; l < 2; l++)
          val += F[Tensor2D::MAP[i][k]]*(*this)[MAP[k][l]]*F[Tensor2D::MAP[j][l]];
      tau[MAP[i][j]] = val;
    }
  tau[3] = F[4]*(*this)[3]*F[4];
  return tau;
}
SymTensor2D SymTensor2D::contravariantPull(const Tensor2D& F) const {
  // S = F^-1*t*F^-T
  double dummy;
  Tensor2D FInv = F.inverse(dummy);
  return contravariantPush(FInv);
}

// compute inverse
SymTensor2D SymTensor2D::inverse(double& det) const {
  SymTensor2D A;
  det = this->determinant();
  double detInv = (*this)[3]/det;
  A[0] =  (*this)[2]*detInv;
  A[1] = -(*this)[1]*detInv;
  A[2] =  (*this)[0]*detInv;
  A[3] =  1.e0/(*this)[3];
  return A;
}

// compute exponential
SymTensor2D SymTensor2D::exp(SymTensor2D dExp[],SymTensor2D d2Exp[][4],
                             bool first,bool second) const {
  unsigned int i,j,k;
  double vA[3],vExpA[3],dExpA[3][3],d2ExpA[3][3][3];
  SymTensor2D expA;
  
  for (i=0; i < 3; i++) vA[i] = (*this)[i];
  double norm = vA[0]*vA[0]+vA[1]*vA[1]+vA[2]*vA[2];
  int test;
  if (norm < 2.25) {
    test = expsym2_series(vA,vExpA,dExpA,d2ExpA,first,second);
    if (!test)
      test = expsym2_spectral(vA,vExpA,dExpA,d2ExpA,first,second);
  }
  else
    test = expsym2_spectral(vA,vExpA,dExpA,d2ExpA,first,second);
  if (!test)
    test = expsym2_linear(vA,vExpA,dExpA,d2ExpA,first,second);
  if (!test) throw ZException("SymTensor2D::exp()");

  for (i=0; i < 3; i++) expA[i] = vExpA[i];
  expA[3] = std::exp((*this)[3]);
  if (first) {
    for (i=0; i < 3; i++)
      for (j=0; j < 3; j++) dExp[i][j] = dExpA[i][j];
    dExp[0][3] = dExp[1][3] = dExp[2][3] = 0.0e0;
    dExp[3][0] = dExp[3][1] = dExp[3][2] = 0.0e0;
    dExp[3][3] = expA[3];
  }
  if (second) {
    for (i=0; i < 3; i++)
      for (j=0; j < 3; j++)
        for (k=0; k < 3; k++) d2Exp[i][j][k] = d2ExpA[i][j][k];
    d2Exp[0][0][3] = d2Exp[0][1][3] = d2Exp[0][2][3] = 0.0e0;
    d2Exp[1][0][3] = d2Exp[1][1][3] = d2Exp[1][2][3] = 0.0e0;
    d2Exp[2][0][3] = d2Exp[2][1][3] = d2Exp[2][2][3] = 0.0e0;
    d2Exp[0][3][0] = d2Exp[1][3][0] = d2Exp[2][3][0] = 0.0e0;
    d2Exp[0][3][1] = d2Exp[1][3][1] = d2Exp[2][3][1] = 0.0e0;
    d2Exp[0][3][2] = d2Exp[1][3][2] = d2Exp[2][3][2] = 0.0e0;
    d2Exp[3][0][0] = d2Exp[3][1][0] = d2Exp[3][2][0] = 0.0e0;
    d2Exp[3][0][1] = d2Exp[3][1][1] = d2Exp[3][2][1] = 0.0e0;
    d2Exp[3][0][2] = d2Exp[3][1][2] = d2Exp[3][2][2] = 0.0e0;
    d2Exp[0][3][3] = d2Exp[1][3][3] = d2Exp[2][3][3] = 0.0e0;
    d2Exp[3][0][3] = d2Exp[3][1][3] = d2Exp[3][2][3] = 0.0e0;
    d2Exp[3][3][0] = d2Exp[3][3][1] = d2Exp[3][3][2] = 0.0e0;
    d2Exp[3][3][3] = expA[3];
  }
  
  return expA;
}

// compute logarithm
SymTensor2D SymTensor2D::log(SymTensor2D dLog[],SymTensor2D d2Log[][4],
                             bool first,bool second) const {
  unsigned int i,j,k;
  double vA[3],vLogA[3],dLogA[3][3],d2LogA[3][3][3];
  SymTensor2D logA;
  
  for (i=0; i < 3; i++) vA[i] = (*this)[i];
  double norm = (vA[0]-1.0)*(vA[0]-1.0)+vA[1]*vA[1]+(vA[2]-1.0)*(vA[2]-1.0);
  int test;
  if (norm < 2.25) {
    test = logsym2_series(vA,vLogA,dLogA,d2LogA,first,second);
    if (!test)
      test = logsym2_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  }
  else
    test = logsym2_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  if (!test) throw ZException("SymTensor2D::log()");

  for (i=0; i < 3; i++) logA[i] = vLogA[i];
  logA[3] = std::log((*this)[3]);
  if (first) {
    for (i=0; i < 3; i++)
      for (j=0; j < 3; j++) dLog[i][j] = dLogA[i][j];
    dLog[0][3] = dLog[1][3] = dLog[2][3] = 0.0e0;
    dLog[3][0] = dLog[3][1] = dLog[3][2] = 0.0e0;
    dLog[3][3] = 1.0e0/(*this)[3];
  }
  if (second) {
    for (i=0; i < 3; i++)
      for (j=0; j < 3; j++)
        for (k=0; k < 3; k++) d2Log[i][j][k] = d2LogA[i][j][k];
    d2Log[0][0][3] = d2Log[0][1][3] = d2Log[0][2][3] = 0.0e0;
    d2Log[1][0][3] = d2Log[1][1][3] = d2Log[1][2][3] = 0.0e0;
    d2Log[2][0][3] = d2Log[2][1][3] = d2Log[2][2][3] = 0.0e0;
    d2Log[0][3][0] = d2Log[1][3][0] = d2Log[2][3][0] = 0.0e0;
    d2Log[0][3][1] = d2Log[1][3][1] = d2Log[2][3][1] = 0.0e0;
    d2Log[0][3][2] = d2Log[1][3][2] = d2Log[2][3][2] = 0.0e0;
    d2Log[3][0][0] = d2Log[3][1][0] = d2Log[3][2][0] = 0.0e0;
    d2Log[3][0][1] = d2Log[3][1][1] = d2Log[3][2][1] = 0.0e0;
    d2Log[3][0][2] = d2Log[3][1][2] = d2Log[3][2][2] = 0.0e0;
    d2Log[0][3][3] = d2Log[1][3][3] = d2Log[2][3][3] = 0.0e0;
    d2Log[3][0][3] = d2Log[3][1][3] = d2Log[3][2][3] = 0.0e0;
    d2Log[3][3][0] = d2Log[3][3][1] = d2Log[3][3][2] = 0.0e0;
    d2Log[3][3][3] = -1.0e0/((*this)[3]*(*this)[3]);
  }
  
  return logA;
}

// identity tensor
SymTensor2D SymTensor2D::identity() {
  SymTensor2D I;
  I[0] = I[2] = I[3] = 1.0e0;
  I[1] = 0.0e0;
  return I;
}

// build tensor by vector outer product
SymTensor2D SymTensor2D::outerProd(const Vector3D& v) {
  SymTensor2D A;
  A[0] = v[0]*v[0];
  A[1] = v[0]*v[1];
  A[2] = v[1]*v[1];
  A[3] = v[2]*v[2];
  return A;
}
SymTensor2D SymTensor2D::outerProd(const Vector2D& v) {
  SymTensor2D A;
  A[0] = v[0]*v[0];
  A[1] = v[0]*v[1];
  A[2] = v[1]*v[1];
  A[3] = 0.0e0;
  return A;
}
SymTensor2D SymTensor2D::outerProd(const Vector3D& a,const Vector3D& b) {
  SymTensor2D C;
  C[0] = a[0]*b[0];
  C[1] = 0.5*(a[0]*b[1]+a[1]*b[0]);
  C[2] = a[1]*b[1];
  C[3] = a[2]*b[2];
  return C;
}
SymTensor2D SymTensor2D::outerProd(const Vector2D& a,const Vector2D& b) {
  SymTensor2D C;
  C[0] = a[0]*b[0];
  C[1] = 0.5*(a[0]*b[1]+a[1]*b[0]);
  C[2] = a[1]*b[1];
  C[3] = 0.0e0;
  return C;
}

// export as square matrix
ShortSqrMatrix SymTensor2D::toMatrix() const {
  ShortSqrMatrix M(3);
  M = 0.e0;
  M[0][0] = (*this)[0];
  M[0][1] = M[1][0] = (*this)[1];
  M[1][1] = (*this)[2];
  M[2][2] = (*this)[3];
  return M;
}

// full inner product
double MATLIB_NAMESPACE innerProd2(const SymTensor2D& A,const SymTensor2D& B) {
  return A[0]*B[0]+A[2]*B[2]+A[3]*B[3]+(A[1]*B[1])*2;
}

// symmetric part of product between two symmetric tensors
SymTensor2D MATLIB_NAMESPACE symProd(const SymTensor2D& A,const SymTensor2D& B) {
  SymTensor2D C;
  C[0] = A[0]*B[0]+A[1]*B[1];
  C[1] = 0.5*(A[0]*B[1]+A[1]*B[2]+A[1]*B[0]+A[2]*B[1]);
  C[2] = A[1]*B[1]+A[2]*B[2];
  C[3] = A[3]*B[3];
  return C;
}


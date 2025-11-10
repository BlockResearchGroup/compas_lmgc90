/*
 *  $Id: SymTensor1D.cpp 130 2013-04-11 01:18:02Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "SymTensor1D.h"

// std C library
#include <cmath>
// local
#include <math/Vector1D.h>
#include <math/Vector3D.h>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for StdSymTensor1D.
 */

// index map
const int StdSymTensor1D::MAP[1][1] = {{0}};

// specific arithmetic operators
/*StdSymTensor1D StdSymTensor1D::operator*(const StdSymTensor1D& A) const {
  StdSymTensor1D B;
  B[0] = (*this)[0]*A[0];
  return B;
}*/

// compute determinant
double StdSymTensor1D::determinant() const {
  return (*this)[0];
}

// compute eigenvalues
void StdSymTensor1D::eigenValues(double v[]) const {
  v[0] = (*this)[0];
}

// compute eigenvalues and eigenbases
void StdSymTensor1D::eigenSplit(double v[],StdSymTensor1D M[]) const {
  v[0] = (*this)[0];
  M[0][0] = 1.0e0;
}

// compute trace
double StdSymTensor1D::trace() const {
  return (*this)[0];
}

// push-pull operations
StdSymTensor1D StdSymTensor1D::covariantPush(const StdSymTensor1D& F) const {
  // g = F^-T*C*F^-1
  double FInv = 1.0e0/F[0];
  StdSymTensor1D g;
  g[0] = FInv*(*this)[0]*FInv;
  return g;
}
StdSymTensor1D StdSymTensor1D::covariantPull(const StdSymTensor1D& F) const {
  // C = F^T*g*F
  StdSymTensor1D C;
  C[0] = F[0]*(*this)[0]*F[0];
  return C;
}
StdSymTensor1D StdSymTensor1D::contravariantPush(const StdSymTensor1D& F) const {
  // t = F*S*F^T
  StdSymTensor1D T;
  T[0] = F[0]*(*this)[0]*F[0];
  return T;
}
StdSymTensor1D StdSymTensor1D::contravariantPull(const StdSymTensor1D& F) const {
  // S = F^-1*t*F^-T
  double FInv = 1.0e0/F[0];
  StdSymTensor1D S;
  S[0] = FInv*(*this)[0]*FInv;
  return S;
}

// compute inverse
StdSymTensor1D StdSymTensor1D::inverse(double& det) const {
  StdSymTensor1D A;
  det = this->determinant();
  A[0] = 1.0e0/(*this)[0];
  return A;
}

// compute exponential
StdSymTensor1D StdSymTensor1D::exp(StdSymTensor1D dExp[],StdSymTensor1D d2Exp[][1],
                                   bool first,bool second) const {
  StdSymTensor1D expA;
  expA[0] = std::exp((*this)[0]);
  if (first) dExp[0][0] = (*this)[0];
  if (second) d2Exp[0][0][0] = (*this)[0];
  return expA;
}

// compute logarithm
StdSymTensor1D StdSymTensor1D::log(StdSymTensor1D dLog[],StdSymTensor1D d2Log[][1],
                                   bool first,bool second) const {
  StdSymTensor1D logA;
  logA[0] = std::log((*this)[0]);
  if (first) dLog[0][0] = 1.0e0/(*this)[0];
  if (second) d2Log[0][0][0] = -1.0e0/((*this)[0]*(*this)[0]);
  return logA;
                                   }

// identity tensor
StdSymTensor1D StdSymTensor1D::identity() {
  StdSymTensor1D I;
  I[0] = 1.0e0;
  return I;
}

// build tensor by vector outer product
StdSymTensor1D StdSymTensor1D::outerProd(const Vector1D& v) {
  StdSymTensor1D A;
  A[0] = v[0]*v[0];
  return A;
}
StdSymTensor1D StdSymTensor1D::outerProd(const Vector1D& a,const Vector1D& b) {
  StdSymTensor1D C;
  C[0] = a[0]*b[0];
  return C;
}

// export as square matrix
ShortSqrMatrix StdSymTensor1D::toMatrix() const {
  ShortSqrMatrix M(1);
  M[0][0] = (*this)[0];
  return M;
}

// full inner product
double MATLIB_NAMESPACE innerProd2(const StdSymTensor1D& A,const StdSymTensor1D& B) {
  return A[0]*B[0];
}


/*
 * Methods for SymTensor1D.
 */

// index map
const int SymTensor1D::MAP[3][3] = {{ 0,-1,-1},{-1, 1,-1},{-1,-1, 2}};

// specific arithmetic operators
SymTensor1D SymTensor1D::operator*(const SymTensor1D& A) const {
  SymTensor1D B;
  B[0] = (*this)[0]*A[0];
  B[1] = (*this)[1]*A[1];
  B[2] = (*this)[2]*A[2];
  return B;
}
Vector1D SymTensor1D::operator*(const Vector1D& a) const {
  Vector1D b;
  b[0] = (*this)[0]*a[0];
  return b;
}
Vector3D SymTensor1D::operator*(const Vector3D& a) const {
  Vector3D b;
  b[0] = (*this)[0]*a[0];
  b[1] = (*this)[1]*a[1];
  b[2] = (*this)[2]*a[2];
  return b;
}

// compute determinant
double SymTensor1D::determinant() const {
  return (*this)[0]*(*this)[1]*(*this)[2];
}

// compute eigenvalues
void SymTensor1D::eigenValues(double v[]) const {
  v[0] = (*this)[0];
  v[1] = (*this)[1];
  v[2] = (*this)[2];
}

// compute eigenvalues and eigenbases
void SymTensor1D::eigenSplit(double v[],SymTensor1D M[]) const {
  v[0] = (*this)[0];
  M[0][0] = 1.0e0; M[0][1] = 0.0e0; M[0][2] = 0.0e0;
  v[1] = (*this)[1];
  M[1][0] = 0.0e0; M[1][1] = 1.0e0; M[1][2] = 0.0e0;
  v[2] = (*this)[2];
  M[2][0] = 0.0e0; M[2][1] = 0.0e0; M[2][2] = 1.0e0;
}

// compute trace
double SymTensor1D::trace() const {
  return (*this)[0]+(*this)[1]+(*this)[2];
}

// push-pull operations
SymTensor1D SymTensor1D::covariantPush(const SymTensor1D& F) const {
  // g = F^-T*C*F^-1
  double dummy;
  SymTensor1D FInv = F.inverse(dummy);
  return covariantPull(FInv);
}
SymTensor1D SymTensor1D::covariantPull(const SymTensor1D& F) const {
  // C = F^T*g*F
  SymTensor1D C;
  C[0] = F[0]*(*this)[0]*F[0];
  C[1] = F[1]*(*this)[1]*F[1];
  C[2] = F[2]*(*this)[2]*F[2];
  return C;
}
SymTensor1D SymTensor1D::contravariantPush(const SymTensor1D& F) const {
  // t = F*S*F^T
  SymTensor1D T;
  T[0] = F[0]*(*this)[0]*F[0];
  T[1] = F[1]*(*this)[1]*F[1];
  T[2] = F[2]*(*this)[2]*F[2];
  return T;
}
SymTensor1D SymTensor1D::contravariantPull(const SymTensor1D& F) const {
  // S = F^-1*t*F^-T
  double dummy;
  SymTensor1D FInv = F.inverse(dummy);
  return contravariantPush(FInv);
}

// compute inverse
SymTensor1D SymTensor1D::inverse(double& det) const {
  SymTensor1D A;
  det = this->determinant();
  A[0] = 1.0e0/(*this)[0];
  A[1] = 1.0e0/(*this)[1];
  A[2] = 1.0e0/(*this)[2];
  return A;
}

// compute exponential
SymTensor1D SymTensor1D::exp(SymTensor1D dExp[],SymTensor1D d2Exp[][3],
                             bool first,bool second) const {
  SymTensor1D expA;
  
  expA[0] = std::exp((*this)[0]);
  expA[1] = std::exp((*this)[1]);
  expA[2] = std::exp((*this)[2]);
  if (first) {
    dExp[0][0] = (*this)[0]; dExp[1][0] = dExp[2][0] = 0.0e0;
    dExp[1][1] = (*this)[1]; dExp[0][1] = dExp[2][1] = 0.0e0;
    dExp[2][2] = (*this)[2]; dExp[0][2] = dExp[1][2] = 0.0e0;
  }
  if (second) {
    d2Exp[0][0][0] = (*this)[0]; 
    d2Exp[0][1][0] = d2Exp[0][2][0] = 0.0e0;
    d2Exp[1][0][0] = d2Exp[1][1][0] = d2Exp[1][2][0] = 0.0e0;
    d2Exp[2][0][0] = d2Exp[2][1][0] = d2Exp[2][2][0] = 0.0e0;
    d2Exp[0][0][1] = d2Exp[0][1][1] = d2Exp[0][2][1] = 0.0e0;
    d2Exp[1][1][1] = (*this)[1]; 
    d2Exp[1][0][1] = d2Exp[1][2][1] = 0.0e0;
    d2Exp[2][0][1] = d2Exp[2][1][1] = d2Exp[2][2][1] = 0.0e0;
    d2Exp[0][0][2] = d2Exp[0][1][2] = d2Exp[0][2][2] = 0.0e0;
    d2Exp[1][0][2] = d2Exp[1][1][2] = d2Exp[1][2][2] = 0.0e0;
    d2Exp[2][2][2] = (*this)[2]; 
    d2Exp[2][0][2] = d2Exp[2][1][2] = 0.0e0;
  }
  return expA;
}

// compute logarithm
SymTensor1D SymTensor1D::log(SymTensor1D dLog[],SymTensor1D d2Log[][3],
                             bool first,bool second) const {
  SymTensor1D logA;

  logA[0] = std::log((*this)[0]);
  logA[1] = std::log((*this)[1]);
  logA[2] = std::log((*this)[2]);
  if (first) {
    dLog[0][0] = 1.0e0/(*this)[0]; dLog[1][0] = dLog[2][0] = 0.0e0;
    dLog[1][1] = 1.0e0/(*this)[1]; dLog[0][1] = dLog[2][1] = 0.0e0;
    dLog[2][2] = 1.0e0/(*this)[2]; dLog[0][2] = dLog[1][2] = 0.0e0;
  }
  if (second) {
    d2Log[0][0][0] = -1.0e0/((*this)[0]*(*this)[0]); 
    d2Log[0][1][0] = d2Log[0][2][0] = 0.0e0;
    d2Log[1][0][0] = d2Log[1][1][0] = d2Log[1][2][0] = 0.0e0;
    d2Log[2][0][0] = d2Log[2][1][0] = d2Log[2][2][0] = 0.0e0;
    d2Log[0][0][1] = d2Log[0][1][1] = d2Log[0][2][1] = 0.0e0;
    d2Log[1][1][1] = -1.0e0/((*this)[1]*(*this)[1]); 
    d2Log[1][0][1] = d2Log[1][2][1] = 0.0e0;
    d2Log[2][0][1] = d2Log[2][1][1] = d2Log[2][2][1] = 0.0e0;
    d2Log[0][0][2] = d2Log[0][1][2] = d2Log[0][2][2] = 0.0e0;
    d2Log[1][0][2] = d2Log[1][1][2] = d2Log[1][2][2] = 0.0e0;
    d2Log[2][2][2] = -1.0e0/((*this)[2]*(*this)[2]); 
    d2Log[2][0][2] = d2Log[2][1][2] = 0.0e0;
  }
  return logA;
}

// identity tensor
SymTensor1D SymTensor1D::identity() {
  SymTensor1D I;
  I[0] = I[1] = I[2] = 1.0e0;
  return I;
}

// build tensor by vector outer product
SymTensor1D SymTensor1D::outerProd(const Vector3D& v) {
  SymTensor1D A;
  A[0] = v[0]*v[0];
  A[1] = v[1]*v[1];
  A[2] = v[2]*v[2];
  return A;
}
SymTensor1D SymTensor1D::outerProd(const Vector1D& v) {
  SymTensor1D A;
  A[0] = v[0]*v[0];
  A[1] = 0.0e0;
  A[2] = 0.0e0;
  return A;
}
SymTensor1D SymTensor1D::outerProd(const Vector3D& a,const Vector3D& b) {
  SymTensor1D C;
  C[0] = a[0]*b[0];
  C[1] = a[1]*b[1];
  C[2] = a[2]*b[2];
  return C;
}
SymTensor1D SymTensor1D::outerProd(const Vector1D& a,const Vector1D& b) {
  SymTensor1D C;
  C[0] = a[0]*b[0];
  C[1] = 0.0e0;
  C[2] = 0.0e0;
  return C;
}

// export as square matrix
ShortSqrMatrix SymTensor1D::toMatrix() const {
  ShortSqrMatrix M(3);
  M = 0.e0;
  M[0][0] = (*this)[0];
  M[1][1] = (*this)[1];
  M[2][2] = (*this)[2];
  return M;
}

// full inner product
double MATLIB_NAMESPACE innerProd2(const SymTensor1D& A,const SymTensor1D& B) {
  return A[0]*B[0]+A[1]*B[1]+A[2]*B[2];
}

// symmetric part of product between two symmetric tensors
SymTensor1D MATLIB_NAMESPACE symProd(const SymTensor1D& A,const SymTensor1D& B) {
  SymTensor1D C;
  C[0] = A[0]*B[0];
  C[1] = A[1]*B[1];
  C[2] = A[2]*B[2];
  return C;
}

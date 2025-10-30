/*
 *  $Id: SymTensor3D.cpp 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "SymTensor3D.h"

// std C library
#include <cmath>
// local
#include <math/Tensor3D.h>
#include <math/Vector3D.h>
#include "eigsym.h"
extern int expsym3_series(const double[],double[],double[][6],double[][6][6],bool,bool);
extern int expsym3_spectral(const double[],double[],double[][6],double[][6][6],bool,bool);
extern int expsym3_linear(const double[],double[],double[][6],double[][6][6],bool,bool);
extern int logsym3_series(const double[],double[],double[][6],double[][6][6],bool,bool);
extern int logsym3_spectral(const double[],double[],double[][6],double[][6][6],bool,bool);
extern int logsym3_linear(const double[],double[],double[][6],double[][6][6],bool,bool);

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for SymTensor3D.
 */

// index map
const int SymTensor3D::MAP[3][3] = {{ 0, 1, 3},{ 1, 2, 4},{ 3, 4, 5}};

// specific arithmetic operators
Tensor3D SymTensor3D::operator*(const Tensor3D& A) const {
  Tensor3D B;
  B[0] = (*this)[0]*A[0]+(*this)[1]*A[3]+(*this)[3]*A[6];
  B[1] = (*this)[0]*A[1]+(*this)[1]*A[4]+(*this)[3]*A[7];
  B[2] = (*this)[0]*A[2]+(*this)[1]*A[5]+(*this)[3]*A[8];
  B[3] = (*this)[1]*A[0]+(*this)[2]*A[3]+(*this)[4]*A[6];
  B[4] = (*this)[1]*A[1]+(*this)[2]*A[4]+(*this)[4]*A[7];
  B[5] = (*this)[1]*A[2]+(*this)[2]*A[5]+(*this)[4]*A[8];
  B[6] = (*this)[3]*A[0]+(*this)[4]*A[3]+(*this)[5]*A[6];
  B[7] = (*this)[3]*A[1]+(*this)[4]*A[4]+(*this)[5]*A[7];
  B[8] = (*this)[3]*A[2]+(*this)[4]*A[5]+(*this)[5]*A[8];
  return B;
}
Vector3D SymTensor3D::operator*(const Vector3D& a) const {
  Vector3D b;
  b[0] = (*this)[0]*a[0]+(*this)[1]*a[1]+(*this)[3]*a[2];
  b[1] = (*this)[1]*a[0]+(*this)[2]*a[1]+(*this)[4]*a[2];
  b[2] = (*this)[3]*a[0]+(*this)[4]*a[1]+(*this)[5]*a[2];
  return b;
}

// compute determinant
double SymTensor3D::determinant() const {
  return (*this)[0]*((*this)[2]*(*this)[5]-(*this)[4]*(*this)[4])
        -(*this)[1]*((*this)[1]*(*this)[5]-(*this)[3]*(*this)[4])
        +(*this)[3]*((*this)[1]*(*this)[4]-(*this)[3]*(*this)[2]);
}

// compute eigenvalues
void SymTensor3D::eigenValues(double v[]) const {
  
  // compute invariants
  SymTensor3D C2;
  C2[0] = (*this)[0]*(*this)[0]+(*this)[1]*(*this)[1]+(*this)[3]*(*this)[3];
  C2[1] = (*this)[1]*(*this)[0]+(*this)[2]*(*this)[1]+(*this)[4]*(*this)[3];
  C2[2] = (*this)[1]*(*this)[1]+(*this)[2]*(*this)[2]+(*this)[4]*(*this)[4];
  C2[3] = (*this)[3]*(*this)[0]+(*this)[4]*(*this)[1]+(*this)[5]*(*this)[3];
  C2[4] = (*this)[3]*(*this)[1]+(*this)[4]*(*this)[2]+(*this)[5]*(*this)[4];
  C2[5] = (*this)[3]*(*this)[3]+(*this)[4]*(*this)[4]+(*this)[5]*(*this)[5];
  double I1 = this->trace();
  double I2 = 0.5*(I1*I1-C2.trace());
  double I3 = this->determinant();
  
  // compute eigenvalues
  static const double PI = 4*std::atan(1.e0);
  static const double THIRD = 1.e0/3.e0;
  double x1,x2,x3;
  double b = I2-I1*I1*THIRD;
  double c = -2.*I1*I1*I1/27.+I1*I2*THIRD-I3;
  if (std::fabs(b) < 1.e-12*I2) {
    if (c >= 0.0e0)
      x1 = x2 = x3 = -std::pow(c,THIRD);
    else
      x1 = x2 = x3 = std::pow(-c,THIRD);
  }
  else {
    double d = 2*std::sqrt(-b*THIRD);
    double e = 3*c/(d*b);
    double t = std::atan2(std::sqrt(1.-e*e),e)*THIRD;
    x1 = d*std::cos(t);
    x2 = d*std::cos(t+2*THIRD*PI);
    x3 = d*std::cos(t+4*THIRD*PI);
  }
  v[0] = x1+I1*THIRD;
  v[1] = x2+I1*THIRD;
  v[2] = x3+I1*THIRD;
}

// compute eigenvalues and eigenbases
void SymTensor3D::eigenSplit(double v[],SymTensor3D M[]) const {
  
  double vA[6],eigVec[3][3];
  for (unsigned int ij=0; ij < 6; ij++) vA[ij] = (*this)[ij];
  int test = eigsym3(vA,v,eigVec);
  if (!test) throw ZException("SymTensor3D::eigenSplit()");
  
  for (unsigned int k=0; k < 3; k++) {
    M[k][0] = eigVec[k][0]*eigVec[k][0];
    M[k][1] = eigVec[k][0]*eigVec[k][1];
    M[k][2] = eigVec[k][1]*eigVec[k][1];
    M[k][3] = eigVec[k][0]*eigVec[k][2];
    M[k][4] = eigVec[k][1]*eigVec[k][2];
    M[k][5] = eigVec[k][2]*eigVec[k][2];
  }
}

// compute trace
double SymTensor3D::trace() const {
  return (*this)[0]+(*this)[2]+(*this)[5];
}

// transform to covariant
SymTensor3D SymTensor3D::covariant() const {
  SymTensor3D A(*this);
  A[1] *= 2.0;
  A[3] *= 2.0;
  A[4] *= 2.0;
  return A;
}

// transform to contravariant
SymTensor3D SymTensor3D::contravariant() const {
  SymTensor3D A(*this);
  A[1] *= 0.5;
  A[3] *= 0.5;
  A[4] *= 0.5;
  return A;
}

// push-pull operations
SymTensor3D SymTensor3D::covariantPush(const Tensor3D& F) const {
  // g = F^-T*C*F^-1
  double dummy;
  Tensor3D FInv = F.inverse(dummy);
  return covariantPull(FInv);
}
SymTensor3D SymTensor3D::covariantPull(const Tensor3D& F) const {
  // C = F^T*g*F
  SymTensor3D C;
  unsigned int i,j,k,l;
  for (i=0; i < 3; i++)
    for (j=0; j < 3; j++) {
      double val = 0.0e0;
      for (k=0; k < 3; k++)
        for (l=0; l < 3; l++)
          val += F[Tensor3D::MAP[k][i]]*(*this)[MAP[k][l]]*F[Tensor3D::MAP[l][j]];
      C[MAP[i][j]] = val;
    }
  return C;
}
SymTensor3D SymTensor3D::contravariantPush(const Tensor3D& F) const {
  // t = F*S*F^T
  SymTensor3D tau;
  unsigned int i,j,k,l;
  for (i=0; i < 3; i++)
    for (j=0; j < 3; j++) {
      double val = 0.0e0;
      for (k=0; k < 3; k++)
        for (l=0; l < 3; l++)
          val += F[Tensor3D::MAP[i][k]]*(*this)[MAP[k][l]]*F[Tensor3D::MAP[j][l]];
      tau[MAP[i][j]] = val;
    }
  return tau;
}
SymTensor3D SymTensor3D::contravariantPull(const Tensor3D& F) const {
  // S = F^-1*t*F^-T
  double dummy;
  Tensor3D FInv = F.inverse(dummy);
  return contravariantPush(FInv);
}

// compute inverse
SymTensor3D SymTensor3D::inverse(double& det) const {
  SymTensor3D A;
  det = this->determinant();
  double detInv = 1.e0/det;
  A[0] =  ((*this)[2]*(*this)[5]-(*this)[4]*(*this)[4])*detInv;
  A[1] = -((*this)[1]*(*this)[5]-(*this)[4]*(*this)[3])*detInv;
  A[2] =  ((*this)[0]*(*this)[5]-(*this)[3]*(*this)[3])*detInv;
  A[3] =  ((*this)[1]*(*this)[4]-(*this)[2]*(*this)[3])*detInv;
  A[4] = -((*this)[0]*(*this)[4]-(*this)[3]*(*this)[1])*detInv;
  A[5] =  ((*this)[0]*(*this)[2]-(*this)[1]*(*this)[1])*detInv;
  return A;
}

// compute exponential
SymTensor3D SymTensor3D::exp(SymTensor3D dExp[],SymTensor3D d2Exp[][6],
                             bool first,bool second) const {
  unsigned int i,j,k;
  double vA[6],vExpA[6],dExpA[6][6],d2ExpA[6][6][6];
  SymTensor3D expA;
  
  for (i=0; i < 6; i++) vA[i] = (*this)[i];
  double norm = vA[0]*vA[0]
               +vA[1]*vA[1]+vA[2]*vA[2]
               +vA[3]*vA[3]+vA[4]*vA[4]+vA[5]*vA[5];
  int test;
  if (norm < 2.25) {
    test = expsym3_series(vA,vExpA,dExpA,d2ExpA,first,second);
    if (!test)
      test = expsym3_spectral(vA,vExpA,dExpA,d2ExpA,first,second);
  }
  else 
    test = expsym3_spectral(vA,vExpA,dExpA,d2ExpA,first,second);
  if (!test) // OUCH! OUCH! OUCH!
    test = expsym3_linear(vA,vExpA,dExpA,d2ExpA,first,second);
  if (!test) throw ZException("SymTensor3D::exp()");
  
  for (i=0; i < 6; i++) expA[i] = vExpA[i];
  if (first) {
    for (i=0; i < 6; i++)
      for (j=0; j < 6; j++) dExp[i][j] = dExpA[i][j];
  }
  if (second) {
    for (i=0; i < 6; i++)
      for (j=0; j < 6; j++)
        for (k=0; k < 6; k++) d2Exp[i][j][k] = d2ExpA[i][j][k];
  }
  
  return expA;
}

// compute logarithm
SymTensor3D SymTensor3D::log(SymTensor3D dLog[],SymTensor3D d2Log[][6],
                             bool first,bool second) const {
  unsigned int i,j,k;
  double vA[6],vLogA[6],dLogA[6][6],d2LogA[6][6][6];
  SymTensor3D logA;
  
  for (i=0; i < 6; i++) vA[i] = (*this)[i];
  double norm = (vA[0]-1.0)*(vA[0]-1.0)
                +vA[1]*vA[1]+(vA[2]-1.0)*(vA[2]-1.0)
                +vA[3]*vA[3]+vA[4]*vA[4]+(vA[5]-1.0)*(vA[5]-1.0);
  int test;
  if (norm < 0.04) {
    test = logsym3_series(vA,vLogA,dLogA,d2LogA,first,second);
    if (!test)
      test = logsym3_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  }
  else
    test = logsym3_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  if (!test)
    test = logsym3_linear(vA,vLogA,dLogA,d2LogA,first,second);
  if (!test) throw ZException("SymTensor3D::log()");

  for (i=0; i < 6; i++) logA[i] = vLogA[i];
  if (first) {
    for (i=0; i < 6; i++) 
      for (j=0; j < 6; j++) dLog[i][j] = dLogA[i][j];
  }
  if (second) {
    for (i=0; i < 6; i++)
      for (j=0; j < 6; j++)
        for (k=0; k < 6; k++) d2Log[i][j][k] = d2LogA[i][j][k];
  }
  
  return logA;
}

// identity tensor
SymTensor3D SymTensor3D::identity() {
  SymTensor3D I;
  I[0] = I[2] = I[5] = 1.0e0;
  I[1] = I[3] = I[4] = 0.0e0;
  return I;
}

// build tensor by vector outer product
SymTensor3D SymTensor3D::outerProd(const Vector3D& v) {
  SymTensor3D A;
  A[0] = v[0]*v[0];
  A[1] = v[0]*v[1];
  A[2] = v[1]*v[1];
  A[3] = v[0]*v[2];
  A[4] = v[1]*v[2];
  A[5] = v[2]*v[2];
  return A;
}
SymTensor3D SymTensor3D::outerProd(const Vector3D& a,const Vector3D& b) {
  SymTensor3D C;
  C[0] = a[0]*b[0];
  C[1] = 0.5*(a[0]*b[1]+a[1]*b[0]);
  C[2] = a[1]*b[1];
  C[3] = 0.5*(a[0]*b[2]+a[2]*b[0]);
  C[4] = 0.5*(a[1]*b[2]+a[2]*b[1]);
  C[5] = a[2]*b[2];
  return C;
}

// export as square matrix
ShortSqrMatrix SymTensor3D::toMatrix() const {
  ShortSqrMatrix M(3);
  M[0][0] = (*this)[0];
  M[0][1] = M[1][0] = (*this)[1];
  M[1][1] = (*this)[2];
  M[0][2] = M[2][0] = (*this)[3];
  M[1][2] = M[2][1] = (*this)[4];
  M[2][2] = (*this)[5];
  return M;
}

// full inner product
double MATLIB_NAMESPACE innerProd2(const SymTensor3D& A,const SymTensor3D& B) {
  return A[0]*B[0]+A[2]*B[2]+A[5]*B[5]+(A[1]*B[1]+A[3]*B[3]+A[4]*B[4])*2;
}

// symmetric part of product between two symmetric tensors
SymTensor3D MATLIB_NAMESPACE symProd(const SymTensor3D& A,const SymTensor3D& B) {
  SymTensor3D C;
  C[0] = A[0]*B[0]+A[1]*B[1]+A[3]*B[3];
  C[1] = 0.5*(A[0]*B[1]+A[1]*B[2]+A[3]*B[4]+A[1]*B[0]+A[2]*B[1]+A[4]*B[3]);
  C[2] = A[1]*B[1]+A[2]*B[2]+A[4]*B[4];
  C[3] = 0.5*(A[0]*B[3]+A[1]*B[4]+A[3]*B[5]+A[3]*B[0]+A[4]*B[1]+A[5]*B[3]);
  C[4] = 0.5*(A[1]*B[3]+A[2]*B[4]+A[4]*B[5]+A[3]*B[1]+A[4]*B[2]+A[5]*B[4]);
  C[5] = A[3]*B[3]+A[4]*B[4]+A[5]*B[5];
  return C;
}


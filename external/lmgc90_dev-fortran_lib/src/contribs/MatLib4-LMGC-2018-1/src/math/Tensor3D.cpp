/*
 *  $Id: Tensor3D.cpp 130 2013-04-11 01:18:02Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "Tensor3D.h"

// std C library
#include <cmath>
// local
#include <data/Chronometer.h>
#include <math/SymTensor3D.h>
#include <math/Vector3D.h>
extern int expmat3_series(const double[],double[],double[][9],double[][9][9],bool,bool);
extern int expmat3_spectral(const double[],double[],double[][9],double[][9][9],bool,bool);
extern int expmat3_linear(const double[],double[],double[][9],double[][9][9],bool,bool);
extern int logmat3_series(const double[],double[],double[][9],double[][9][9],bool,bool);
extern int logmat3_spectral(const double[],double[],double[][9],double[][9][9],bool,bool);
extern int logmat3_linear(const double[],double[],double[][9],double[][9][9],bool,bool);

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for Tensor3D.
 */

// index map
const int Tensor3D::MAP[3][3] = {{ 0, 1, 2},{ 3, 4, 5},{ 6, 7, 8}};

// specific arithmetic operators
Tensor3D Tensor3D::operator*(const Tensor3D& A) const {
  Tensor3D B;
  B[0] = (*this)[0]*A[0]+(*this)[1]*A[3]+(*this)[2]*A[6];
  B[1] = (*this)[0]*A[1]+(*this)[1]*A[4]+(*this)[2]*A[7];
  B[2] = (*this)[0]*A[2]+(*this)[1]*A[5]+(*this)[2]*A[8];
  B[3] = (*this)[3]*A[0]+(*this)[4]*A[3]+(*this)[5]*A[6];
  B[4] = (*this)[3]*A[1]+(*this)[4]*A[4]+(*this)[5]*A[7];
  B[5] = (*this)[3]*A[2]+(*this)[4]*A[5]+(*this)[5]*A[8];
  B[6] = (*this)[6]*A[0]+(*this)[7]*A[3]+(*this)[8]*A[6];
  B[7] = (*this)[6]*A[1]+(*this)[7]*A[4]+(*this)[8]*A[7];
  B[8] = (*this)[6]*A[2]+(*this)[7]*A[5]+(*this)[8]*A[8];
  return B;
}
Tensor3D Tensor3D::operator*(const SymTensor3D& A) const {
  Tensor3D B;
  B[0] = (*this)[0]*A[0]+(*this)[1]*A[1]+(*this)[2]*A[3];
  B[1] = (*this)[0]*A[1]+(*this)[1]*A[2]+(*this)[2]*A[4];
  B[2] = (*this)[0]*A[3]+(*this)[1]*A[4]+(*this)[2]*A[5];
  B[3] = (*this)[3]*A[0]+(*this)[4]*A[1]+(*this)[5]*A[3];
  B[4] = (*this)[3]*A[1]+(*this)[4]*A[2]+(*this)[5]*A[4];
  B[5] = (*this)[3]*A[3]+(*this)[4]*A[4]+(*this)[5]*A[5];
  B[6] = (*this)[6]*A[0]+(*this)[7]*A[1]+(*this)[8]*A[3];
  B[7] = (*this)[6]*A[1]+(*this)[7]*A[2]+(*this)[8]*A[4];
  B[8] = (*this)[6]*A[3]+(*this)[7]*A[4]+(*this)[8]*A[5];
  return B;
}
Vector3D Tensor3D::operator*(const Vector3D& a) const {
  Vector3D b;
  b[0] = (*this)[0]*a[0]+(*this)[1]*a[1]+(*this)[2]*a[2];
  b[1] = (*this)[3]*a[0]+(*this)[4]*a[1]+(*this)[5]*a[2];
  b[2] = (*this)[6]*a[0]+(*this)[7]*a[1]+(*this)[8]*a[2];
  return b;
}

// symmetrize (use this operation on strains)
SymTensor3D Tensor3D::covariantSym() const {
  SymTensor3D S;
  S[0] = (*this)[0];
  S[1] = (*this)[1]+(*this)[3];
  S[2] = (*this)[4];
  S[3] = (*this)[2]+(*this)[6];
  S[4] = (*this)[5]+(*this)[7];
  S[5] = (*this)[8];
  return S;
}

// symmetrize (use this operation on stresses)
SymTensor3D Tensor3D::contravariantSym() const {
  SymTensor3D S;
  S[0] = (*this)[0];
  S[1] = 0.5*((*this)[1]+(*this)[3]);
  S[2] = (*this)[4];
  S[3] = 0.5*((*this)[2]+(*this)[6]);
  S[4] = 0.5*((*this)[5]+(*this)[7]);
  S[5] = (*this)[8];
  return S;
}

// compute determinant
double Tensor3D::determinant() const {
  return (*this)[0]*((*this)[4]*(*this)[8]-(*this)[5]*(*this)[7])
        -(*this)[1]*((*this)[3]*(*this)[8]-(*this)[5]*(*this)[6])
        +(*this)[2]*((*this)[3]*(*this)[7]-(*this)[4]*(*this)[6]);
}

// compute trace
double Tensor3D::trace() const {
  return (*this)[0]+(*this)[4]+(*this)[8];
}

// compute inverse
Tensor3D Tensor3D::inverse(double& det) const {
  Tensor3D A;
  det = this->determinant();
  double detInv = 1.e0/det;
  A[0] =  ((*this)[4]*(*this)[8]-(*this)[5]*(*this)[7])*detInv;
  A[1] = -((*this)[1]*(*this)[8]-(*this)[2]*(*this)[7])*detInv;
  A[2] =  ((*this)[1]*(*this)[5]-(*this)[2]*(*this)[4])*detInv;
  A[3] = -((*this)[3]*(*this)[8]-(*this)[5]*(*this)[6])*detInv;
  A[4] =  ((*this)[0]*(*this)[8]-(*this)[2]*(*this)[6])*detInv;
  A[5] = -((*this)[0]*(*this)[5]-(*this)[2]*(*this)[3])*detInv;
  A[6] =  ((*this)[3]*(*this)[7]-(*this)[4]*(*this)[6])*detInv;
  A[7] = -((*this)[0]*(*this)[7]-(*this)[1]*(*this)[6])*detInv;
  A[8] =  ((*this)[0]*(*this)[4]-(*this)[1]*(*this)[3])*detInv;
  return A;
}

// compute transposed
Tensor3D Tensor3D::transposed() const {
  Tensor3D A;
  A[0] = (*this)[0];
  A[1] = (*this)[3];
  A[2] = (*this)[6];
  A[3] = (*this)[1];
  A[4] = (*this)[4];
  A[5] = (*this)[7];
  A[6] = (*this)[2];
  A[7] = (*this)[5];
  A[8] = (*this)[8];
  return A;
}

// tensor exponential
Tensor3D Tensor3D::exp(Tensor3D dExp[],Tensor3D d2Exp[][9],
                       bool first,bool second) const {
  unsigned int i,j,k;
  double vA[9],vExpA[9],dExpA[9][9],d2ExpA[9][9][9];
  Tensor3D expA;
#ifdef EXP_TIME
  static Chronometer clck;
  clck.start();
#endif
  
  for (i=0; i < 9; i++) vA[i] = (*this)[i];
#ifdef SLU
  int test = expmat3_linear(vA,vExpA,dExpA,d2ExpA,first,second);
#else
  double norm = vA[0]*vA[0]+vA[1]*vA[1]+vA[2]*vA[2]
               +vA[3]*vA[3]+vA[4]*vA[4]+vA[5]*vA[5]
               +vA[6]*vA[6]+vA[7]*vA[7]+vA[8]*vA[8];
  int test;
  if (norm < 2.25) {
    test = expmat3_series(vA,vExpA,dExpA,d2ExpA,first,second);
    if (!test) // OUCH!
      test = expmat3_spectral(vA,vExpA,dExpA,d2ExpA,first,second);
  }
  else
    test = expmat3_spectral(vA,vExpA,dExpA,d2ExpA,first,second);
  if (!test) // OUCH! OUCH! OUCH!
    test = expmat3_linear(vA,vExpA,dExpA,d2ExpA,first,second);
#endif
  if (!test) {
    std::cout << (*this) << std::endl;
    throw ZException("irrecoverable error in Tensor3D::exp()");
  }
  
  for (i=0; i < 9; i++) expA[i] = vExpA[i];
  if (first) {
    for (i=0; i < 9; i++)
      for (j=0; j < 9; j++) dExp[i][j] = dExpA[i][j];
  }
  if (second) {
    for (i=0; i < 9; i++)
      for (j=0; j < 9; j++)
        for (k=0; k < 9; k++) d2Exp[i][j][k] = d2ExpA[i][j][k];
  }
  
#ifdef EXP_TIME
  clck.stop();
  std::cout << "exp time: " << Chronometer::toString(clck.elapsed()) << std::endl;
#endif

  return expA;
}

// tensor logarithm
Tensor3D Tensor3D::log(Tensor3D dLog[],Tensor3D d2Log[][9],
                       bool first,bool second) const {
  unsigned int i,j,k;
  double vA[9],vLogA[9],dLogA[9][9],d2LogA[9][9][9];
  Tensor3D logA;
  
  for (i=0; i < 9; i++) vA[i] = (*this)[i];
  double norm = (vA[0]-1.0)*(vA[0]-1.0)+vA[1]*vA[1]+vA[2]*vA[2]
               +vA[3]*vA[3]+(vA[4]-1.0)*(vA[4]-1.0)+vA[5]*vA[5]
               +vA[6]*vA[6]+vA[7]*vA[7]+(vA[8]-1.0)*(vA[8]-1.0);
  int test;
  if (norm < 0.04) {
    test = logmat3_series(vA,vLogA,dLogA,d2LogA,first,second);
    if (!test) // OUCH!
      test = logmat3_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  }
  else
    test = logmat3_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  if (!test) // OUCH! OUCH! OUCH!
    test = logmat3_linear(vA,vLogA,dLogA,d2LogA,first,second);
  if (!test) throw ZException("Tensor3D::log()");
  
  for (i=0; i < 9; i++) logA[i] = vLogA[i];
  if (first) {
    for (i=0; i < 9; i++)
      for (j=0; j < 9; j++) dLog[i][j] = dLogA[i][j];
  }
  if (second) {
    for (i=0; i < 9; i++)
      for (j=0; j < 9; j++)
        for (k=0; k < 9; k++) d2Log[i][j][k] = d2LogA[i][j][k];
  }
  
  return logA;
}

// identity tensor
Tensor3D Tensor3D::identity() {
  Tensor3D I;
  I[0] = I[4] = I[8] = 1.0e0;
  I[1] = I[2] = I[3] = 0.0e0;
  I[5] = I[6] = I[7] = 0.0e0;
  return I;
}

// export as square matrix
ShortSqrMatrix Tensor3D::toMatrix() const {
  ShortSqrMatrix M(3);
  M[0][0] = (*this)[0];
  M[0][1] = (*this)[1];
  M[0][2] = (*this)[2];
  M[1][0] = (*this)[3];
  M[1][1] = (*this)[4];
  M[1][2] = (*this)[5];
  M[2][0] = (*this)[6];
  M[2][1] = (*this)[7];
  M[2][2] = (*this)[8];
  return M;
}


/*
 *  $Id: TensorAlgebra1D.cpp 158 2015-01-10 21:14:05Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "TensorAlgebra.h"

// std C library
#include <cmath>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for TensorAlgebra1D.
 */

// SymTensor operations
TensorAlgebra1D::ARRAY TensorAlgebra1D::SymTensor::identity() {
  ARRAY I(SymTensor::MEMSIZE);
  I[0] = I[1] = I[2] = 1.e0;
  return I;
}
void TensorAlgebra1D::SymTensor::cofactor(const ARRAY& A,ARRAY& Ac) {
  Ac[0] = A[1]*A[2];
  Ac[1] = A[0]*A[2];
  Ac[2] = A[0]*A[1];
}
double TensorAlgebra1D::SymTensor::inverse(const ARRAY& A,ARRAY& Ainv) {
  double detA = determinant(A);
  Ainv[0] = 1.e0/A[0];
  Ainv[1] = 1.e0/A[1];
  Ainv[2] = 1.e0/A[2];
  return detA;
}
double TensorAlgebra1D::SymTensor::determinant(const ARRAY& A) {
  return A[0]*A[1]*A[2];
}
double TensorAlgebra1D::SymTensor::trace(const ARRAY& A) {
  return A[0]+A[1]+A[2];
}
double TensorAlgebra1D::SymTensor::innerProd(const ARRAY& A,const ARRAY& B) {
  return A[0]*B[0]+A[1]*B[1]+A[2]*B[2];
}
void TensorAlgebra1D::SymTensor::log(const ARRAY& A,ARRAY& logA,ARRAY dLog[],
                                     ARRAY d2Log[][3],bool first,bool second) {
  logA[0] = std::log(A[0]);
  logA[1] = std::log(A[1]);
  logA[2] = std::log(A[2]);
  if (first) {
    dLog[0][0] = 1.0e0/A[0]; dLog[1][0] = dLog[2][0] = 0.0e0;
    dLog[1][1] = 1.0e0/A[1]; dLog[0][1] = dLog[2][1] = 0.0e0;
    dLog[2][2] = 1.0e0/A[2]; dLog[0][2] = dLog[1][2] = 0.0e0;
  }
  if (second) {
    d2Log[0][0][0] = -dLog[0][0]*dLog[0][0]; 
    d2Log[0][1][0] = d2Log[0][2][0] = 0.0e0;
    d2Log[1][0][0] = d2Log[1][1][0] = d2Log[1][2][0] = 0.0e0;
    d2Log[2][0][0] = d2Log[2][1][0] = d2Log[2][2][0] = 0.0e0;
    d2Log[0][0][1] = d2Log[0][1][1] = d2Log[0][2][1] = 0.0e0;
    d2Log[1][1][1] = -dLog[1][1]*dLog[1][1]; 
    d2Log[1][0][1] = d2Log[1][2][1] = 0.0e0;
    d2Log[2][0][1] = d2Log[2][1][1] = d2Log[2][2][1] = 0.0e0;
    d2Log[0][0][2] = d2Log[0][1][2] = d2Log[0][2][2] = 0.0e0;
    d2Log[1][0][2] = d2Log[1][1][2] = d2Log[1][2][2] = 0.0e0;
    d2Log[2][2][2] = -dLog[2][2]*dLog[2][2]; 
    d2Log[2][0][2] = d2Log[2][1][2] = 0.0e0;
  }
}
void TensorAlgebra1D::SymTensor::covariant(const ARRAY& A,ARRAY& covA) {
  covA = A;
}
void TensorAlgebra1D::SymTensor::contravariant(const ARRAY& A,ARRAY& conA) {
  conA = A;
}

// SymTensor4 operations
TensorAlgebra1D::MATRIX TensorAlgebra1D::SymTensor4::identity() {
  MATRIX I(SymTensor::MEMSIZE);
  I = 0.e0;
  I[0][0] = I[1][1] = I[2][2] = 1.e0;
  return I;
}
TensorAlgebra1D::MATRIX TensorAlgebra1D::SymTensor4::contravariantIdentity() {
  return SymTensor4::identity();
}
TensorAlgebra1D::MATRIX TensorAlgebra1D::SymTensor4::covariantIdentity() {
  return SymTensor4::identity();
}
TensorAlgebra1D::MATRIX TensorAlgebra1D::SymTensor4::baseJ() {
  static const double ONE_THIRD = 1.e0/3.e0;
  static const double TWO_THIRD = 2.e0/3.e0;
  MATRIX J(SymTensor::MEMSIZE);
  J[0][0] =  TWO_THIRD; J[0][1] = -ONE_THIRD; J[0][2] = -ONE_THIRD;
  J[1][0] = -ONE_THIRD; J[1][1] =  TWO_THIRD; J[1][2] = -ONE_THIRD;
  J[2][0] = -ONE_THIRD; J[2][1] = -ONE_THIRD; J[2][2] =  TWO_THIRD;
  return J;
}
TensorAlgebra1D::MATRIX TensorAlgebra1D::SymTensor4::baseK() {
  static const double ONE_THIRD = 1.e0/3.e0;
  MATRIX K(SymTensor::MEMSIZE);
  K = ONE_THIRD;
  return K;
}
void TensorAlgebra1D::SymTensor4::addIKJL(double coef,const ARRAY& A,MATRIX& M) {
  // M_ijkl += coef*(A_ik*A_jl+A_il*A_jk)
  double coef2 = coef+coef;
  M[0][0] += coef2*A[0]*A[0];
  M[1][1] += coef2*A[1]*A[1];
  M[2][2] += coef2*A[2]*A[2];
}
void TensorAlgebra1D::SymTensor4::outerProd(const ARRAY& A,MATRIX& M) {
  // M_ijkl = A_ij*A_kl
  for (unsigned int i=0; i < SymTensor::MEMSIZE; i++)
    for (unsigned int j=0; j < SymTensor::MEMSIZE; j++) M[i][j] = A[i]*A[j];
}
void TensorAlgebra1D::SymTensor4::outerProd2(const ARRAY& A,const ARRAY& B,MATRIX& M) {
  // M_ijkl = A_ij*B_kl+B_ij*A_kl
  for (unsigned int i=0; i < SymTensor::MEMSIZE; i++)
    for (unsigned int j=0; j < SymTensor::MEMSIZE; j++) 
      M[i][j] = A[i]*B[j]+B[i]*A[j];
}
void TensorAlgebra1D::SymTensor4::rightProd(const MATRIX& M,const ARRAY& A,ARRAY& B) {
  // B = M.A
  for (unsigned int i=0; i < SymTensor::MEMSIZE; i++) {
    double* p = M[i];
    B[i] = 0.e0;
    for (unsigned int j=0; j < SymTensor::MEMSIZE; j++) B[i] += p[j]*A[j];
  }
}

// Tensor operations
TensorAlgebra1D::ARRAY TensorAlgebra1D::Tensor::identity() {
  ARRAY I(Tensor::MEMSIZE);
  I[0] = I[1] = I[2] = 1.e0;
  return I;
}
void TensorAlgebra1D::Tensor::cofactor(const ARRAY& A,ARRAY& Ac) {
  Ac[0] = A[1]*A[2];
  Ac[1] = A[0]*A[2];
  Ac[2] = A[0]*A[1];
}
double TensorAlgebra1D::Tensor::inverse(const ARRAY& A,ARRAY& Ainv) {
  double detA = determinant(A);
  Ainv[0] = 1.e0/A[0];
  Ainv[1] = 1.e0/A[1];
  Ainv[2] = 1.e0/A[2];
  return detA;
}
double TensorAlgebra1D::Tensor::determinant(const ARRAY& A) {
  return A[0]*A[1]*A[2];
}
double TensorAlgebra1D::Tensor::trace(const ARRAY& A) {
  return A[0]+A[1]+A[2];
}
void TensorAlgebra1D::Tensor::prod(const ARRAY& A,const ARRAY& B,ARRAY& C) {
  C[0] = A[0]*B[0];
  C[1] = A[1]*B[1];
  C[2] = A[2]*B[2];
}
void TensorAlgebra1D::Tensor::log(const ARRAY& A,ARRAY& logA,ARRAY dLog[],
                                  ARRAY d2Log[][3],bool first,bool second) {
  logA[0] = std::log(A[0]);
  logA[1] = std::log(A[1]);
  logA[2] = std::log(A[2]);
  if (first) {
    dLog[0][0] = 1.0e0/A[0]; dLog[1][0] = dLog[2][0] = 0.0e0;
    dLog[1][1] = 1.0e0/A[1]; dLog[0][1] = dLog[2][1] = 0.0e0;
    dLog[2][2] = 1.0e0/A[2]; dLog[0][2] = dLog[1][2] = 0.0e0;
  }
  if (second) {
    d2Log[0][0][0] = -dLog[0][0]*dLog[0][0]; 
    d2Log[0][1][0] = d2Log[0][2][0] = 0.0e0;
    d2Log[1][0][0] = d2Log[1][1][0] = d2Log[1][2][0] = 0.0e0;
    d2Log[2][0][0] = d2Log[2][1][0] = d2Log[2][2][0] = 0.0e0;
    d2Log[0][0][1] = d2Log[0][1][1] = d2Log[0][2][1] = 0.0e0;
    d2Log[1][1][1] = -dLog[1][1]*dLog[1][1]; 
    d2Log[1][0][1] = d2Log[1][2][1] = 0.0e0;
    d2Log[2][0][1] = d2Log[2][1][1] = d2Log[2][2][1] = 0.0e0;
    d2Log[0][0][2] = d2Log[0][1][2] = d2Log[0][2][2] = 0.0e0;
    d2Log[1][0][2] = d2Log[1][1][2] = d2Log[1][2][2] = 0.0e0;
    d2Log[2][2][2] = -dLog[2][2]*dLog[2][2]; 
    d2Log[2][0][2] = d2Log[2][1][2] = 0.0e0;
  }
}
  
// operations
void TensorAlgebra1D::RightCauchyGreen(const ARRAY& F,ARRAY& C) {
  // C=F^t.F
  C[0] = F[0]*F[0];
  C[1] = F[1]*F[1];
  C[2] = F[2]*F[2];
}
void TensorAlgebra1D::RUDecomposition(const ARRAY& F,ARRAY& R,ARRAY& U) {
  // F=R.U
  R[0] = R[1] = R[2] = 1.e0;
  U[0] = F[0];
  U[1] = F[1];
  U[2] = F[2];
}
void TensorAlgebra1D::CauchyToPK1(const ARRAY& sig,const ARRAY& F,ARRAY& P) {
  // P = sig.(J F^-t)
  ARRAY Fc(Tensor::MEMSIZE);
  Tensor::cofactor(F,Fc);
  P[0] = sig[0]*Fc[0];
  P[1] = sig[1]*Fc[1];
  P[2] = sig[2]*Fc[2];
}
void TensorAlgebra1D::PK1ToCauchy(const ARRAY& P,const ARRAY& F,ARRAY& sig) {
  // sig = (1/J) P.F^t
  double J = Tensor::determinant(F);
  double Jinv = 1.e0/J;
  sig[0] = Jinv*P[0]*F[0];
  sig[1] = Jinv*P[1]*F[1];
  sig[2] = Jinv*P[2]*F[2];
}
void TensorAlgebra1D::PK2ToKirchhoff(const ARRAY& S,const ARRAY& F,ARRAY& tau) {
  // tau = F.S.F^t
  tau[0] = F[0]*S[0]*F[0];
  tau[1] = F[1]*S[1]*F[1];
  tau[2] = F[2]*S[2]*F[2];
}
void TensorAlgebra1D::PK2ToPK1(const ARRAY& S,const ARRAY& F,ARRAY& P) {
  // P=F.S
  P[0] = F[0]*S[0];
  P[1] = F[1]*S[1];
  P[2] = F[2]*S[2];
}
void TensorAlgebra1D::MaterialToLagrangian(const MATRIX& M,const ARRAY& S,
                                           const ARRAY& F,MATRIX& T) {
  // T_iJkL=F_iI.M_IJKL.F_kK+S_JL 1_ik
  T[0][0] = F[0]*M[0][0]*F[0]+S[0];
  T[0][1] = F[0]*M[0][1]*F[1];
  T[0][2] = F[0]*M[0][2]*F[2];
  T[1][0] = F[1]*M[1][0]*F[0];
  T[1][1] = F[1]*M[1][1]*F[1]+S[1];
  T[1][2] = F[1]*M[1][2]*F[2];
  T[2][0] = F[2]*M[2][0]*F[0];
  T[2][1] = F[2]*M[2][1]*F[1];
  T[2][2] = F[2]*M[2][2]*F[2]+S[2];
}
void TensorAlgebra1D::SpatialToLagrangian(const MATRIX& M,const ARRAY& sig,
                                          const ARRAY& F,MATRIX& T) {
  // T_iJkL=J F^-1_Jj.(M_ijkl+sig_jl 1_ik).F^-1_Ll
  ARRAY Finv(Tensor::MEMSIZE);
  double J = Tensor::inverse(F,Finv);
  T[0][0] = J*Finv[0]*(M[0][0]+sig[0])*Finv[0];
  T[0][1] = J*Finv[0]*M[0][1]*Finv[1];
  T[0][2] = J*Finv[0]*M[0][2]*Finv[2];
  T[1][0] = J*Finv[1]*M[1][0]*Finv[0];
  T[1][1] = J*Finv[1]*(M[1][1]+sig[1])*Finv[1];
  T[1][2] = J*Finv[1]*M[1][2]*Finv[2];
  T[2][0] = J*Finv[2]*M[2][0]*Finv[0];
  T[2][1] = J*Finv[2]*M[2][1]*Finv[1];
  T[2][2] = J*Finv[2]*(M[2][2]+sig[2])*Finv[2];
}

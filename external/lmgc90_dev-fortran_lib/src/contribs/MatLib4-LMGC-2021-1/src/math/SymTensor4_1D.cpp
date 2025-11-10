/*
 *  $Id: SymTensor4_1D.cpp 137 2013-08-30 15:20:05Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "SymTensor4_1D.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for SymTensor4_1D
 */

// access operators
double SymTensor4_1D::operator()(unsigned int i,unsigned int j,
                                 unsigned int k,unsigned int l) const
 throw (std::out_of_range) {
  if (i < 3 && i == j && k < 3 && k == l)
    return (*this)[SymTensor1D::MAP[i][j]][SymTensor1D::MAP[k][l]];
  else
    throw std::out_of_range("SymTensor4_1D");
}
double& SymTensor4_1D::operator()(unsigned int i,unsigned int j,
                                  unsigned int k,unsigned int l)
 throw (std::out_of_range) {
  if (i < 3 && i == j && k < 3 && k == l)
    return (*this)[SymTensor1D::MAP[i][j]][SymTensor1D::MAP[k][l]];
  else
    throw std::out_of_range("SymTensor4_1D");
}

// specific operation
void SymTensor4_1D::addIJKL(double coef,const SymTensor1D& A) {
  // M_ijkl += coef*(A_ik*A_jl+A_il*A_jk)
  double coef2 = coef+coef;
  (*this)[0][0] += coef2*A[0]*A[0];
  (*this)[1][1] += coef2*A[1]*A[1];
  (*this)[2][2] += coef2*A[2]*A[2];
}
void SymTensor4_1D::addIJKL(double coef,const SymTensor1D& A,const SymTensor1D& B) {
  // M_ijkl += coef*(A_ik*B_jl+A_il*B_jk)
  double coef2 = coef+coef;
  (*this)[0][0] += coef2*A[0]*B[0];
  (*this)[1][1] += coef2*A[1]*B[1];
  (*this)[2][2] += coef2*A[2]*B[2];
}

// push-pull operations
SymTensor4_1D SymTensor4_1D::covariantPush(const SymTensor1D& F) const {
  // S_ijkl = F^-1_Ii F^-1_Jj S_IJKL F^-1_Kk F^-1_Ll
  double dummy;
  SymTensor1D FInv = F.inverse(dummy);
  return covariantPull(FInv);
}
SymTensor4_1D SymTensor4_1D::covariantPull(const SymTensor1D& F) const {
  // S_IJKL = F_iI F_jJ S_ijkl F_kK F_lL
  SymTensor4_1D S;
  S[0][0] = F[0]*F[0]*(*this)[0][0]*F[0]*F[0];
  S[0][1] = F[0]*F[0]*(*this)[0][1]*F[1]*F[1];
  S[0][2] = F[0]*F[0]*(*this)[0][2]*F[2]*F[2];
  S[1][0] = F[1]*F[1]*(*this)[1][0]*F[0]*F[0];
  S[1][1] = F[1]*F[1]*(*this)[1][1]*F[1]*F[1];
  S[1][2] = F[1]*F[1]*(*this)[1][2]*F[2]*F[2];
  S[2][0] = F[2]*F[2]*(*this)[2][0]*F[0]*F[0];
  S[2][1] = F[2]*F[2]*(*this)[2][1]*F[1]*F[1];
  S[2][2] = F[2]*F[2]*(*this)[2][2]*F[2]*F[2];
  return S;
}
SymTensor4_1D SymTensor4_1D::contravariantPush(const SymTensor1D& F) const {
  // M_ijkl = F_iI F_jJ M_IJKL F_kK F_lL
  SymTensor4_1D M;
  M[0][0] = F[0]*F[0]*(*this)[0][0]*F[0]*F[0];
  M[0][1] = F[0]*F[0]*(*this)[0][1]*F[1]*F[1];
  M[0][2] = F[0]*F[0]*(*this)[0][2]*F[2]*F[2];
  M[1][0] = F[1]*F[1]*(*this)[1][0]*F[0]*F[0];
  M[1][1] = F[1]*F[1]*(*this)[1][1]*F[1]*F[1];
  M[1][2] = F[1]*F[1]*(*this)[1][2]*F[2]*F[2];
  M[2][0] = F[2]*F[2]*(*this)[2][0]*F[0]*F[0];
  M[2][1] = F[2]*F[2]*(*this)[2][1]*F[1]*F[1];
  M[2][2] = F[2]*F[2]*(*this)[2][2]*F[2]*F[2];
  return M;
}
SymTensor4_1D SymTensor4_1D::contravariantPull(const SymTensor1D& F) const {
  // M_IJKL = F^-1_Ii F^-1_Jj M_ijkl F^-1_Kk F^-1_Ll
  double dummy;
  SymTensor1D FInv = F.inverse(dummy);
  return contravariantPush(FInv);
}

// identity tensor
SymTensor4_1D SymTensor4_1D::identity() {
  SymTensor4_1D I;
  I = 0.0e0;
  I[0][0] = I[1][1] = I[2][2] = 1.0e0;
  return I;
}
SymTensor4_1D SymTensor4_1D::contravariantIdentity() {
  SymTensor4_1D I;
  I = 0.0e0;
  I[0][0] = I[1][1] = I[2][2] = 1.0e0;
  return I;
}
SymTensor4_1D SymTensor4_1D::covariantIdentity() {
  SymTensor4_1D I;
  I = 0.0e0;
  I[0][0] = I[1][1] = I[2][2] = 1.0e0;
  return I;
}

// tensorial bases
SymTensor4_1D SymTensor4_1D::baseJ() {
  static const double ONE_THIRD = 1.e0/3.e0;
  static const double TWO_THIRD = 2.e0/3.e0;
  SymTensor4_1D J;
  J[0][0] =  TWO_THIRD; J[0][1] = -ONE_THIRD; J[0][2] = -ONE_THIRD;
  J[1][0] = -ONE_THIRD; J[1][1] =  TWO_THIRD; J[1][2] = -ONE_THIRD;
  J[2][0] = -ONE_THIRD; J[2][1] = -ONE_THIRD; J[2][2] =  TWO_THIRD;
  return J;
}
SymTensor4_1D SymTensor4_1D::baseK() {
  static const double ONE_THIRD = 1.e0/3.e0;
  SymTensor4_1D K;
  K = ONE_THIRD;
  return K;
}

// full inner product
SymTensor1D MATLIB_NAMESPACE innerProd2(const SymTensor4_1D& A,
                                        const SymTensor1D& B) {
  SymTensor1D C;
  C[0] = A[0][0]*B[0]+A[0][1]*B[1]+A[0][2]*B[2];
  C[1] = A[1][0]*B[0]+A[1][1]*B[1]+A[1][2]*B[2];
  C[2] = A[2][0]*B[0]+A[2][1]*B[1]+A[2][2]*B[2];
  return C;
}
SymTensor4_1D MATLIB_NAMESPACE innerProd2(const SymTensor4_1D& A,const SymTensor4_1D& B){
  SymTensor4_1D C;
  for (unsigned int kl=0; kl < 4; kl++) {
    C[0][kl] = A[0][0]*B[0][kl]+A[0][1]*B[1][kl]+A[0][2]*B[2][kl];
    C[1][kl] = A[1][0]*B[0][kl]+A[1][1]*B[1][kl]+A[1][2]*B[2][kl];
    C[2][kl] = A[2][0]*B[0][kl]+A[2][1]*B[1][kl]+A[2][2]*B[2][kl];
  }
  return C;
}

// symmetric part of product between symmetric 4th-order and 2nd-order tensors
SymTensor4_1D MATLIB_NAMESPACE symProd(const SymTensor4_1D& A,const SymTensor1D& B) {
  SymTensor4_1D C;
  for (unsigned int i=0; i < 3; i++) {
    double *a = A[i];
    double *c = C[i];
    c[0] = a[0]*B[0];
    c[1] = a[1]*B[1];
    c[2] = a[2]*B[2];
  }
  return C;
}
SymTensor4_1D MATLIB_NAMESPACE symProd(const SymTensor1D& A,const SymTensor4_1D& B) {
  SymTensor4_1D C;
  for (unsigned int i=0; i < 3; i++) {
    C[0][i] = A[0]*B[0][i];
    C[1][i] = A[1]*B[1][i];
    C[2][i] = A[2]*B[2][i];
  }
  return C;
}

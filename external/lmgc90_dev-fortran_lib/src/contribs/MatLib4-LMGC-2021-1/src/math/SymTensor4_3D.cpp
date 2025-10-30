/*
 *  $Id: SymTensor4_3D.cpp 137 2013-08-30 15:20:05Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "SymTensor4_3D.h"

// local
#include <math/Tensor3D.h>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for SymTensor4_3D
 */

// access operators
double SymTensor4_3D::operator()(unsigned int i,unsigned int j,
                                 unsigned int k,unsigned int l) const
 throw (std::out_of_range) {
  if (i < 3 && j < 3 && k < 3 && l < 3)
    return (*this)[SymTensor3D::MAP[i][j]][SymTensor3D::MAP[k][l]];
  else
    throw std::out_of_range("SymTensor4_3D");
}
double& SymTensor4_3D::operator()(unsigned int i,unsigned int j,
                                  unsigned int k,unsigned int l)
 throw (std::out_of_range) {
  if (i < 3 && j < 3 && k < 3 && l < 3)
    return (*this)[SymTensor3D::MAP[i][j]][SymTensor3D::MAP[k][l]];
  else
    throw std::out_of_range("SymTensor4_3D");
}

// specific operation
void SymTensor4_3D::addIJKL(double coef,const SymTensor3D& A) {
  // M_ijkl += coef*(A_ik*A_jl+A_il*A_jk)
  unsigned int i,j,k,l,ij,kl; 
  for (i=0, ij=0; i < 3; i++)
    for (j=0; j <= i; j++, ij++)
      for (k=0, kl=0; k < 3; k++)
        for (l=0; l <= k; l++, kl++)
          (*this)[ij][kl] += coef*(A[SymTensor3D::MAP[i][k]]*A[SymTensor3D::MAP[j][l]]
                                  +A[SymTensor3D::MAP[i][l]]*A[SymTensor3D::MAP[j][k]]);
}
void SymTensor4_3D::addIJKL(double coef,const SymTensor3D& A,const SymTensor3D& B) {
  // M_ijkl += coef*(A_ik*B_jl+A_il*B_jk)
  unsigned int i,j,k,l,ij,kl; 
  for (i=0, ij=0; i < 3; i++)
    for (j=0; j <= i; j++, ij++)
      for (k=0, kl=0; k < 3; k++)
        for (l=0; l <= k; l++, kl++)
          (*this)[ij][kl] += coef*(A[SymTensor3D::MAP[i][k]]*B[SymTensor3D::MAP[j][l]]
                                  +A[SymTensor3D::MAP[i][l]]*B[SymTensor3D::MAP[j][k]]);
}

// push-pull operations
SymTensor4_3D SymTensor4_3D::covariantPush(const Tensor3D& F) const {
  // S_ijkl = F^-1_Ii F^-1_Jj S_IJKL F^-1_Kk F^-1_Ll
  double dummy;
  Tensor3D FInv = F.inverse(dummy);
  return covariantPull(FInv);
}
SymTensor4_3D SymTensor4_3D::covariantPull(const Tensor3D& F) const {
  // S_IJKL = F_iI F_jJ S_ijkl F_kK F_lL
  SymTensor4_3D S;
  unsigned int i,j,k,l,m,n,p,q;
  for (i=0; i < 3; i++)
    for (j=0; j < 3; j++) 
      for (k=0; k < 3; k++)
        for (l=0; l < 3; l++) {
          double val = 0.0e0;
          for (m=0; m < 3; m++)
            for (n=0; n < 3; n++)
              for (p=0; p < 3; p++)
                for (q=0; q < 3; q++)
                  val += F[Tensor3D::MAP[m][i]]*F[Tensor3D::MAP[n][j]]
                         *(*this)[SymTensor3D::MAP[m][n]][SymTensor3D::MAP[p][q]]
                         *F[Tensor3D::MAP[p][k]]*F[Tensor3D::MAP[q][l]];
          S[SymTensor3D::MAP[i][j]][SymTensor3D::MAP[k][l]] = val;
        }
  return S;
}
SymTensor4_3D SymTensor4_3D::contravariantPush(const Tensor3D& F) const {
  // M_ijkl = F_iI F_jJ M_IJKL F_kK F_lL
  SymTensor4_3D M;
  unsigned int i,j,k,l,m,n,p,q;
  for (i=0; i < 3; i++)
    for (j=0; j < 3; j++) 
      for (k=0; k < 3; k++)
        for (l=0; l < 3; l++) {
          double val = 0.0e0;
          for (m=0; m < 3; m++)
            for (n=0; n < 3; n++)
              for (p=0; p < 3; p++)
                for (q=0; q < 3; q++)
                  val += F[Tensor3D::MAP[i][m]]*F[Tensor3D::MAP[j][n]]
                         *(*this)[SymTensor3D::MAP[m][n]][SymTensor3D::MAP[p][q]]
                         *F[Tensor3D::MAP[k][p]]*F[Tensor3D::MAP[l][q]];
          M[SymTensor3D::MAP[i][j]][SymTensor3D::MAP[k][l]] = val;
        }
  return M;
}
SymTensor4_3D SymTensor4_3D::contravariantPull(const Tensor3D& F) const {
  // M_IJKL = F^-1_Ii F^-1_Jj M_ijkl F^-1_Kk F^-1_Ll
  double dummy;
  Tensor3D FInv = F.inverse(dummy);
  return contravariantPush(FInv);
}

// identity tensor
SymTensor4_3D SymTensor4_3D::identity() {
  SymTensor4_3D I;
  I = 0.0e0;
  I[0][0] = I[2][2] = I[5][5] = 1.0e0;
  I[1][1] = I[3][3] = I[4][4] = 1.0e0;
  return I;
}
SymTensor4_3D SymTensor4_3D::contravariantIdentity() {
  SymTensor4_3D I;
  I = 0.0e0;
  I[0][0] = I[2][2] = I[5][5] = 1.0e0;
  I[1][1] = I[3][3] = I[4][4] = 0.5e0;
  return I;
}
SymTensor4_3D SymTensor4_3D::covariantIdentity() {
  SymTensor4_3D I;
  I = 0.0e0;
  I[0][0] = I[2][2] = I[5][5] = 1.0e0;
  I[1][1] = I[3][3] = I[4][4] = 2.0e0;
  return I;
}

// tensorial bases
SymTensor4_3D SymTensor4_3D::baseJ() {
  static const double ONE_THIRD = 1.e0/3.e0;
  static const double TWO_THIRD = 2.e0/3.e0;
  SymTensor4_3D J;
  J = 0.e0;
  J[0][0] =  TWO_THIRD; J[0][2] = -ONE_THIRD; J[0][5] = -ONE_THIRD;
  J[2][0] = -ONE_THIRD; J[2][2] =  TWO_THIRD; J[2][5] = -ONE_THIRD;
  J[5][0] = -ONE_THIRD; J[5][2] = -ONE_THIRD; J[5][5] =  TWO_THIRD;
  J[1][1] = J[3][3] = J[4][4] = 1.0e0;
  return J;
}
SymTensor4_3D SymTensor4_3D::baseK() {
  static const double ONE_THIRD = 1.e0/3.e0;
  SymTensor4_3D K;
  K = 0.e0;
  K[0][0] = ONE_THIRD; K[0][2] = ONE_THIRD; K[0][5] = ONE_THIRD;
  K[2][0] = ONE_THIRD; K[2][2] = ONE_THIRD; K[2][5] = ONE_THIRD;
  K[5][0] = ONE_THIRD; K[5][2] = ONE_THIRD; K[5][5] = ONE_THIRD;
  return K;
}

// full inner product
SymTensor3D MATLIB_NAMESPACE innerProd2(const SymTensor4_3D& A,
                                        const SymTensor3D& B) {
  SymTensor3D C;
  C[0] = A[0][0]*B[0]+A[0][2]*B[2]+A[0][5]*B[5]
        +(A[0][1]*B[1]+A[0][3]*B[3]+A[0][4]*B[4])*2;
  C[1] = A[1][0]*B[0]+A[1][2]*B[2]+A[1][5]*B[5]
        +(A[1][1]*B[1]+A[1][3]*B[3]+A[1][4]*B[4])*2;
  C[2] = A[2][0]*B[0]+A[2][2]*B[2]+A[2][5]*B[5]
        +(A[2][1]*B[1]+A[2][3]*B[3]+A[2][4]*B[4])*2;
  C[3] = A[3][0]*B[0]+A[3][2]*B[2]+A[3][5]*B[5]
        +(A[3][1]*B[1]+A[3][3]*B[3]+A[3][4]*B[4])*2;
  C[4] = A[4][0]*B[0]+A[4][2]*B[2]+A[4][5]*B[5]
        +(A[4][1]*B[1]+A[4][3]*B[3]+A[4][4]*B[4])*2;
  C[5] = A[5][0]*B[0]+A[5][2]*B[2]+A[5][5]*B[5]
        +(A[5][1]*B[1]+A[5][3]*B[3]+A[5][4]*B[4])*2;
  return C;
}
SymTensor4_3D MATLIB_NAMESPACE innerProd2(const SymTensor4_3D& A,const SymTensor4_3D& B){
  SymTensor4_3D C;
  for (unsigned int kl=0; kl < 6; kl++) {
    C[0][kl] = A[0][0]*B[0][kl]+A[0][2]*B[2][kl]+A[0][5]*B[5][kl]
              +(A[0][1]*B[1][kl]+A[0][3]*B[3][kl]+A[0][4]*B[4][kl])*2;
    C[1][kl] = A[1][0]*B[0][kl]+A[1][2]*B[2][kl]+A[1][5]*B[5][kl]
              +(A[1][1]*B[1][kl]+A[1][3]*B[3][kl]+A[1][4]*B[4][kl])*2;
    C[2][kl] = A[2][0]*B[0][kl]+A[2][2]*B[2][kl]+A[2][5]*B[5][kl]
              +(A[2][1]*B[1][kl]+A[2][3]*B[3][kl]+A[2][4]*B[4][kl])*2;
    C[3][kl] = A[3][0]*B[0][kl]+A[3][2]*B[2][kl]+A[3][5]*B[5][kl]
              +(A[3][1]*B[1][kl]+A[3][3]*B[3][kl]+A[3][4]*B[4][kl])*2;
    C[4][kl] = A[4][0]*B[0][kl]+A[4][2]*B[2][kl]+A[4][5]*B[5][kl]
              +(A[4][1]*B[1][kl]+A[4][3]*B[3][kl]+A[4][4]*B[4][kl])*2;
    C[5][kl] = A[5][0]*B[0][kl]+A[5][2]*B[2][kl]+A[5][5]*B[5][kl]
              +(A[5][1]*B[1][kl]+A[5][3]*B[3][kl]+A[5][4]*B[4][kl])*2;
  }
  return C;
}

// symmetric part of product between symmetric 4th-order and 2nd-order tensors
SymTensor4_3D MATLIB_NAMESPACE symProd(const SymTensor4_3D& A,const SymTensor3D& B) {
  SymTensor4_3D C;
  for (unsigned int i=0; i < 6; i++) {
    double *a = A[i];
    double *c = C[i];
    c[0] = a[0]*B[0]+a[1]*B[1]+a[3]*B[3];
    c[1] = 0.5*(a[0]*B[1]+a[1]*B[2]+a[3]*B[4]+a[1]*B[0]+a[2]*B[1]+a[4]*B[3]);
    c[2] = a[1]*B[1]+a[2]*B[2]+a[4]*B[4];
    c[3] = 0.5*(a[0]*B[3]+a[1]*B[4]+a[3]*B[5]+a[3]*B[0]+a[4]*B[1]+a[5]*B[3]);
    c[4] = 0.5*(a[1]*B[3]+a[2]*B[4]+a[4]*B[5]+a[3]*B[1]+a[4]*B[2]+a[5]*B[4]);
    c[5] = a[3]*B[3]+a[4]*B[4]+a[5]*B[5];
  }
  return C;
}
SymTensor4_3D MATLIB_NAMESPACE symProd(const SymTensor3D& A,const SymTensor4_3D& B) {
  SymTensor4_3D C;
  for (unsigned int i=0; i < 6; i++) {
    C[0][i] = A[0]*B[0][i]+A[1]*B[1][i]+A[3]*B[3][i];
    C[1][i] = 0.5*(A[0]*B[1][i]+A[1]*B[2][i]+A[3]*B[4][i]+A[1]*B[0][i]+A[2]*B[1][i]+A[4]*B[3][i]);
    C[2][i] = A[1]*B[1][i]+A[2]*B[2][i]+A[4]*B[4][i];
    C[3][i] = 0.5*(A[0]*B[3][i]+A[1]*B[4][i]+A[3]*B[5][i]+A[3]*B[0][i]+A[4]*B[1][i]+A[5]*B[3][i]);
    C[4][i] = 0.5*(A[1]*B[3][i]+A[2]*B[4][i]+A[4]*B[5][i]+A[3]*B[1][i]+A[4]*B[2][i]+A[5]*B[4][i]);
    C[5][i] = A[3]*B[3][i]+A[4]*B[4][i]+A[5]*B[5][i];
  }
  return C;
}

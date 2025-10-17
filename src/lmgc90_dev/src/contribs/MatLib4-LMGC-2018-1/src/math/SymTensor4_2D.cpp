/*
 *  $Id: SymTensor4_2D.cpp 137 2013-08-30 15:20:05Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "SymTensor4_2D.h"

// local
#include <math/Tensor2D.h>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for SymTensor4_2D
 */

// access operators
double SymTensor4_2D::operator()(unsigned int i,unsigned int j,
                                 unsigned int k,unsigned int l) const
 throw (std::out_of_range) {
  if (((i < 2 && j < 2) || (i < 3 && i == j)) && ((k < 2 && l < 2) || (k < 3 && k == l)))
    return (*this)[SymTensor2D::MAP[i][j]][SymTensor2D::MAP[k][l]];
  else
    throw std::out_of_range("SymTensor4_2D");
}
double& SymTensor4_2D::operator()(unsigned int i,unsigned int j,
                                  unsigned int k,unsigned int l)
 throw (std::out_of_range) {
  if (((i < 2 && j < 2) || i == j) && ((k < 2 && l < 2) || k == l))
    return (*this)[SymTensor2D::MAP[i][j]][SymTensor2D::MAP[k][l]];
  else
    throw std::out_of_range("SymTensor4_2D");
}

// specific operation
void SymTensor4_2D::addIJKL(double coef,const SymTensor2D& A) {
  // M_ijkl += coef*(A_ik*A_jl+A_il*A_jk)
  unsigned int i,j,k,l,ij,kl; 
  for (i=0, ij=0; i < 2; i++)
    for (j=0; j <= i; j++, ij++)
      for (k=0, kl=0; k < 2; k++)
        for (l=0; l <= k; l++, kl++)
          (*this)[ij][kl] += coef*(A[SymTensor2D::MAP[i][k]]*A[SymTensor2D::MAP[j][l]]
                                  +A[SymTensor2D::MAP[i][l]]*A[SymTensor2D::MAP[j][k]]);
  double coef2 = coef+coef;
  (*this)[3][3] += coef2*A[3]*A[3];
}
void SymTensor4_2D::addIJKL(double coef,const SymTensor2D& A,const SymTensor2D& B) {
  // M_ijkl += coef*(A_ik*B_jl+A_il*B_jk)
  unsigned int i,j,k,l,ij,kl; 
  for (i=0, ij=0; i < 2; i++)
    for (j=0; j <= i; j++, ij++)
      for (k=0, kl=0; k < 2; k++)
        for (l=0; l <= k; l++, kl++)
          (*this)[ij][kl] += coef*(A[SymTensor2D::MAP[i][k]]*B[SymTensor2D::MAP[j][l]]
                                  +A[SymTensor2D::MAP[i][l]]*B[SymTensor2D::MAP[j][k]]);
  double coef2 = coef+coef;
  (*this)[3][3] += coef2*A[3]*B[3];
}

// push-pull operations
SymTensor4_2D SymTensor4_2D::covariantPush(const Tensor2D& F) const {
  // S_ijkl = F^-1_Ii F^-1_Jj S_IJKL F^-1_Kk F^-1_Ll
  double dummy;
  Tensor2D FInv = F.inverse(dummy);
  return covariantPull(FInv);
}
SymTensor4_2D SymTensor4_2D::covariantPull(const Tensor2D& F) const {
  // S_IJKL = F_iI F_jJ S_ijkl F_kK F_lL
  SymTensor4_2D S;
  unsigned int i,j,k,l,m,n,p,q;
  for (i=0; i < 2; i++)
    for (j=0; j < 2; j++) {
      for (k=0; k < 2; k++)
        for (l=0; l < 2; l++) {
          double val = 0.0e0;
          for (m=0; m < 2; m++)
            for (n=0; n < 2; n++)
              for (p=0; p < 2; p++)
                for (q=0; q < 2; q++)
                  val += F[Tensor2D::MAP[m][i]]*F[Tensor2D::MAP[n][j]]
                    *(*this)[SymTensor2D::MAP[m][n]][SymTensor2D::MAP[p][q]]
                    *F[Tensor2D::MAP[p][k]]*F[Tensor2D::MAP[q][l]];
          S[SymTensor2D::MAP[i][j]][SymTensor2D::MAP[k][l]] = val;
        }
      double val1 = 0.0e0;
      double val2 = 0.0e0;
      for (m=0; m < 2; m++)
        for (n=0; n < 2; n++) {
          val1 += F[Tensor2D::MAP[m][i]]*F[Tensor2D::MAP[n][j]]
                  *(*this)[SymTensor2D::MAP[m][n]][3]*F[4]*F[4];
          val2 += F[4]*F[4]*(*this)[3][SymTensor2D::MAP[m][n]]
                  *F[Tensor2D::MAP[m][i]]*F[Tensor2D::MAP[n][j]];
        }
      S[SymTensor2D::MAP[i][j]][3] = val1;
      S[3][SymTensor2D::MAP[i][j]] = val2;
    }
  S[3][3] = F[4]*F[4]*(*this)[3][3]*F[4]*F[4];
  return S;
}
SymTensor4_2D SymTensor4_2D::contravariantPush(const Tensor2D& F) const {
  // M_ijkl = F_iI F_jJ M_IJKL F_kK F_lL
  SymTensor4_2D M;
  unsigned int i,j,k,l,m,n,p,q;
  for (i=0; i < 2; i++)
    for (j=0; j < 2; j++) {
      for (k=0; k < 2; k++)
        for (l=0; l < 2; l++) {
          double val = 0.0e0;
          for (m=0; m < 2; m++)
            for (n=0; n < 2; n++)
              for (p=0; p < 2; p++)
                for (q=0; q < 2; q++)
                  val += F[Tensor2D::MAP[i][m]]*F[Tensor2D::MAP[j][n]]
                         *(*this)[SymTensor2D::MAP[m][n]][SymTensor2D::MAP[p][q]]
                         *F[Tensor2D::MAP[k][p]]*F[Tensor2D::MAP[l][q]];
          M[SymTensor2D::MAP[i][j]][SymTensor2D::MAP[k][l]] = val;
        }
      double val1 = 0.0e0;
      double val2 = 0.0e0;
      for (m=0; m < 2; m++)
        for (n=0; n < 2; n++) {
          val1 += F[Tensor2D::MAP[i][m]]*F[Tensor2D::MAP[j][n]]
                  *(*this)[SymTensor2D::MAP[m][n]][3]*F[4]*F[4];
          val2 += F[4]*F[4]*(*this)[3][SymTensor2D::MAP[m][n]]
                  *F[Tensor2D::MAP[i][m]]*F[Tensor2D::MAP[j][n]];
        }
      M[SymTensor2D::MAP[i][j]][3] = val1;
      M[3][SymTensor2D::MAP[i][j]] = val2;
    }
    M[3][3] = F[4]*F[4]*(*this)[3][3]*F[4]*F[4];
  return M;
}
SymTensor4_2D SymTensor4_2D::contravariantPull(const Tensor2D& F) const {
  // M_IJKL = F^-1_Ii F^-1_Jj M_ijkl F^-1_Kk F^-1_Ll
  double dummy;
  Tensor2D FInv = F.inverse(dummy);
  return contravariantPush(FInv);
}

// identity tensor
SymTensor4_2D SymTensor4_2D::identity() {
  SymTensor4_2D I;
  I = 0.0e0;
  I[0][0] = I[2][2] = I[3][3] = 1.0e0;
  I[1][1] = 1.0e0;
  return I;
}
SymTensor4_2D SymTensor4_2D::contravariantIdentity() {
  SymTensor4_2D I;
  I = 0.0e0;
  I[0][0] = I[2][2] = I[3][3] = 1.0e0;
  I[1][1] = 0.5e0;
  return I;
}
SymTensor4_2D SymTensor4_2D::covariantIdentity() {
  SymTensor4_2D I;
  I = 0.0e0;
  I[0][0] = I[2][2] = I[3][3] = 1.e0;
  I[1][1] = 2.0e0;
  return I;
}

// tensorial bases
SymTensor4_2D SymTensor4_2D::baseJ() {
  static const double ONE_THIRD = 1.e0/3.e0;
  static const double TWO_THIRD = 2.e0/3.e0;
  SymTensor4_2D J;
  J = 0.e0;
  J[0][0] =  TWO_THIRD; J[0][2] = -ONE_THIRD; J[0][3] = -ONE_THIRD;
  J[2][0] = -ONE_THIRD; J[2][2] =  TWO_THIRD; J[2][3] = -ONE_THIRD;
  J[3][0] = -ONE_THIRD; J[3][2] = -ONE_THIRD; J[3][3] =  TWO_THIRD;
  J[1][1] = 1.0e0;
  return J;
}
SymTensor4_2D SymTensor4_2D::baseK() {
  static const double ONE_THIRD = 1.e0/3.e0;
  SymTensor4_2D K;
  K = 0.e0;
  K[0][0] = ONE_THIRD; K[0][2] = ONE_THIRD; K[0][3] = ONE_THIRD;
  K[2][0] = ONE_THIRD; K[2][2] = ONE_THIRD; K[2][3] = ONE_THIRD;
  K[3][0] = ONE_THIRD; K[3][2] = ONE_THIRD; K[3][3] = ONE_THIRD;
  return K;
}

// full inner product
SymTensor2D MATLIB_NAMESPACE innerProd2(const SymTensor4_2D& A,
                                        const SymTensor2D& B) {
  SymTensor2D C;
  C[0] = A[0][0]*B[0]+A[0][2]*B[2]+A[0][3]*B[3]+(A[0][1]*B[1])*2;
  C[1] = A[1][0]*B[0]+A[1][2]*B[2]+A[1][3]*B[3]+(A[1][1]*B[1])*2;
  C[2] = A[2][0]*B[0]+A[2][2]*B[2]+A[2][3]*B[3]+(A[2][1]*B[1])*2;
  C[3] = A[3][0]*B[0]+A[3][2]*B[2]+A[3][3]*B[3]+(A[3][1]*B[1])*2;
  return C;
}
SymTensor4_2D MATLIB_NAMESPACE innerProd2(const SymTensor4_2D& A,const SymTensor4_2D& B){
  SymTensor4_2D C;
  for (unsigned int kl=0; kl < 4; kl++) {
    C[0][kl] = A[0][0]*B[0][kl]+A[0][2]*B[2][kl]+A[0][3]*B[3][kl]+(A[0][1]*B[1][kl])*2;
    C[1][kl] = A[1][0]*B[0][kl]+A[1][2]*B[2][kl]+A[1][3]*B[3][kl]+(A[1][1]*B[1][kl])*2;
    C[2][kl] = A[2][0]*B[0][kl]+A[2][2]*B[2][kl]+A[2][3]*B[3][kl]+(A[2][1]*B[1][kl])*2;
    C[3][kl] = A[3][0]*B[0][kl]+A[3][2]*B[2][kl]+A[3][3]*B[3][kl]+(A[3][1]*B[1][kl])*2;
  }
  return C;
}

// symmetric part of product between symmetric 4th-order and 2nd-order tensors
SymTensor4_2D MATLIB_NAMESPACE symProd(const SymTensor4_2D& A,const SymTensor2D& B) {
  SymTensor4_2D C;
  for (unsigned int i=0; i < 4; i++) {
    double *a = A[i];
    double *c = C[i];
    c[0] = a[0]*B[0]+a[1]*B[1];
    c[1] = 0.5*(a[0]*B[1]+a[1]*B[2]+a[1]*B[0]+a[2]*B[1]);
    c[2] = a[1]*B[1]+a[2]*B[2];
    c[3] = a[3]*B[3];
  }
  return C;
}
SymTensor4_2D MATLIB_NAMESPACE symProd(const SymTensor2D& A,const SymTensor4_2D& B) {
  SymTensor4_2D C;
  for (unsigned int i=0; i < 4; i++) {
    C[0][i] = A[0]*B[0][i]+A[1]*B[1][i];
    C[1][i] = 0.5*(A[0]*B[1][i]+A[1]*B[2][i]+A[1]*B[0][i]+A[2]*B[1][i]);
    C[2][i] = A[1]*B[1][i]+A[2]*B[2][i];
    C[3][i] = A[3]*B[3][i];
  }
  return C;
}

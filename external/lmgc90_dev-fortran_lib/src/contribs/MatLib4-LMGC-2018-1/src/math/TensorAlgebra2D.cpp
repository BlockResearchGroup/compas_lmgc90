/*
 *  $Id: TensorAlgebra2D.cpp 158 2015-01-10 21:14:05Z lstainier $
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
// local
extern int logsym2_series(const double[],double[],double[][3],double[][3][3],bool,bool);
extern int logsym2_spectral(const double[],double[],double[][3],double[][3][3],bool,bool);
extern int logmat2_series(const double[],double[],double[][4],double[][4][4],bool,bool);
extern int logmat2_spectral(const double[],double[],double[][4],double[][4][4],bool,bool);

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for TensorAlgebra2D.
 */
static const int MAP1[2][2] = {{0,1},{1,2}};
static const int MAP2[2][2] = {{0,1},{2,3}};

// SymTensor operations
TensorAlgebra2D::ARRAY TensorAlgebra2D::SymTensor::identity() {
  ARRAY I(SymTensor::MEMSIZE);
  I[0] = I[2] = I[3] = 1.e0;
  I[1] = 0.e0;
  return I;
}
void TensorAlgebra2D::SymTensor::cofactor(const ARRAY& A,ARRAY& Ac) {
  Ac[0] =  A[2]*A[3];
  Ac[1] = -A[1]*A[3];
  Ac[2] =  A[0]*A[3];
  Ac[3] =  A[0]*A[2]-A[1]*A[1];
}
double TensorAlgebra2D::SymTensor::inverse(const ARRAY& A,ARRAY& Ainv) {
  double detA = determinant(A);
  double detInv = A[3]/detA;
  Ainv[0] =  A[2]*detInv;
  Ainv[1] = -A[1]*detInv;
  Ainv[2] =  A[0]*detInv;
  Ainv[3] =  1.e0/A[3];
  return detA;
}
double TensorAlgebra2D::SymTensor::determinant(const ARRAY& A) {
  return (A[0]*A[2]-A[1]*A[1])*A[3];
}
double TensorAlgebra2D::SymTensor::trace(const ARRAY& A) {
  return A[0]+A[2]+A[3];
}
double TensorAlgebra2D::SymTensor::innerProd(const ARRAY& A,const ARRAY& B) {
  return A[0]*B[0]+A[2]*B[2]+A[3]*B[3]+(A[1]*B[1])*2;
}
void TensorAlgebra2D::SymTensor::log(const ARRAY& A,ARRAY& logA,ARRAY dLog[],
                                     ARRAY d2Log[][4],bool first,bool second) {
  unsigned int i,j,k;
  double vA[3],vLogA[3],dLogA[3][3],d2LogA[3][3][3];
  
  for (i=0; i < 3; i++) vA[i] = A[i];
  double norm = (vA[0]-1.0)*(vA[0]-1.0)+vA[1]*vA[1]+(vA[2]-1.0)*(vA[2]-1.0);
  if (norm < 2.25) {
    int test = logsym2_series(vA,vLogA,dLogA,d2LogA,first,second);
    if (!test)
      logsym2_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  }
  else
    logsym2_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  
  for (i=0; i < 3; i++) logA[i] = vLogA[i];
  logA[3] = std::log(A[3]);
  if (first) {
    for (i=0; i < 3; i++)
      for (j=0; j < 3; j++) dLog[i][j] = dLogA[i][j];
    dLog[0][3] = dLog[1][3] = dLog[2][3] = 0.0e0;
    dLog[3][0] = dLog[3][1] = dLog[3][2] = 0.0e0;
    dLog[3][3] = 1.0e0/A[3];
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
    d2Log[3][3][3] = -dLog[3][3]*dLog[3][3];
  }
}
void TensorAlgebra2D::SymTensor::covariant(const ARRAY& A,ARRAY& covA) {
  covA[0] = A[0];
  covA[1] = A[1]+A[1];
  covA[2] = A[2];
  covA[3] = A[3];
}
void TensorAlgebra2D::SymTensor::contravariant(const ARRAY& A,ARRAY& conA) {
  conA[0] =     A[0];
  conA[1] = 0.5*A[1];
  conA[2] =     A[2];
  conA[3] =     A[3];
}

// SymTensor4 operations
TensorAlgebra2D::MATRIX TensorAlgebra2D::SymTensor4::identity() {
  MATRIX I(SymTensor::MEMSIZE);
  I = 0.0e0;
  I[0][0] = I[2][2] = I[3][3] = 1.0e0;
  I[1][1] = 1.0e0;
  return I;
}
TensorAlgebra2D::MATRIX TensorAlgebra2D::SymTensor4::contravariantIdentity() {
  MATRIX I(SymTensor::MEMSIZE);
  I = 0.0e0;
  I[0][0] = I[2][2] = I[3][3] = 1.0e0;
  I[1][1] = 0.5e0;
  return I;
}
TensorAlgebra2D::MATRIX TensorAlgebra2D::SymTensor4::covariantIdentity() {
  MATRIX I(SymTensor::MEMSIZE);
  I = 0.e0;
  I[0][0] = I[2][2] = I[3][3] = 1.e0;
  I[1][1] = 2.0e0;
  return I;
}
TensorAlgebra2D::MATRIX TensorAlgebra2D::SymTensor4::baseJ() {
  static const double ONE_THIRD = 1.e0/3.e0;
  static const double TWO_THIRD = 2.e0/3.e0;
  MATRIX J(SymTensor::MEMSIZE);
  J = 0.e0;
  J[0][0] =  TWO_THIRD; J[0][2] = -ONE_THIRD; J[0][3] = -ONE_THIRD;
  J[2][0] = -ONE_THIRD; J[2][2] =  TWO_THIRD; J[2][3] = -ONE_THIRD;
  J[3][0] = -ONE_THIRD; J[3][2] = -ONE_THIRD; J[3][3] =  TWO_THIRD;
  J[1][1] = 1.0e0;
  return J;
}
TensorAlgebra2D::MATRIX TensorAlgebra2D::SymTensor4::baseK() {
  static const double ONE_THIRD = 1.e0/3.e0;
  MATRIX K(SymTensor::MEMSIZE);
  K = 0.e0;
  K[0][0] = ONE_THIRD; K[0][2] = ONE_THIRD; K[0][3] = ONE_THIRD;
  K[2][0] = ONE_THIRD; K[2][2] = ONE_THIRD; K[2][3] = ONE_THIRD;
  K[3][0] = ONE_THIRD; K[3][2] = ONE_THIRD; K[3][3] = ONE_THIRD;
  return K;
}
void TensorAlgebra2D::SymTensor4::addIKJL(double coef,const ARRAY& A,MATRIX& M) {
  // M_ijkl += coef*(A_ik*A_jl+A_il*A_jk)
  unsigned int i,j,k,l,ij,kl;
  for (i=0, ij=0; i < DIMENSION; i++)
    for (j=0; j <= i; j++, ij++)
      for (k=0, kl=0; k < DIMENSION; k++)
        for (l=0; l <= k; l++, kl++)
          M[ij][kl] += coef*(A[MAP1[i][k]]*A[MAP1[j][l]]
                            +A[MAP1[i][l]]*A[MAP1[j][k]]);
  double coef2 = coef+coef;
  M[3][3] += coef2*A[3]*A[3];
}
void TensorAlgebra2D::SymTensor4::outerProd(const ARRAY& A,MATRIX& M) {
  // M_ijkl = A_ij*A_kl
  for (unsigned int i=0; i < SymTensor::MEMSIZE; i++)
    for (unsigned int j=0; j < SymTensor::MEMSIZE; j++) M[i][j] = A[i]*A[j];
}
void TensorAlgebra2D::SymTensor4::outerProd2(const ARRAY& A,const ARRAY& B,MATRIX& M) {
  // M_ijkl = A_ij*B_kl+B_ij*A_kl
  for (unsigned int i=0; i < SymTensor::MEMSIZE; i++)
    for (unsigned int j=0; j < SymTensor::MEMSIZE; j++) 
      M[i][j] = A[i]*B[j]+B[i]*A[j];
}
void TensorAlgebra2D::SymTensor4::rightProd(const MATRIX& M,const ARRAY& A,ARRAY& B) {
  // B = M.A
  for (unsigned int i=0; i < SymTensor::MEMSIZE; i++) {
    double* p = M[i];
    B[i] = 0.e0;
    for (unsigned int j=0; j < SymTensor::MEMSIZE; j++) B[i] += p[j]*A[j];
  }
}

// Tensor operations
TensorAlgebra2D::ARRAY TensorAlgebra2D::Tensor::identity() {
  ARRAY I(Tensor::MEMSIZE);
  I[0] = I[3] = I[4] = 1.e0;
  I[1] = I[2] = 0.e0;
  return I;
}
void TensorAlgebra2D::Tensor::cofactor(const ARRAY& A,ARRAY& Ac) {
  Ac[0] =  A[3]*A[4];
  Ac[1] = -A[2]*A[4];
  Ac[2] = -A[1]*A[4];
  Ac[3] =  A[0]*A[4];
  Ac[4] =  A[0]*A[3]-A[1]*A[2];
}
double TensorAlgebra2D::Tensor::inverse(const ARRAY& A,ARRAY& Ainv) {
  double detA = determinant(A);
  double detInv = A[4]/detA;
  Ainv[0] =  A[3]*detInv;
  Ainv[1] = -A[1]*detInv;
  Ainv[2] = -A[2]*detInv;
  Ainv[3] =  A[0]*detInv;
  Ainv[4] = 1.e0/A[4];
  return detA;
}
double TensorAlgebra2D::Tensor::determinant(const ARRAY& A) {
  return (A[0]*A[3]-A[1]*A[2])*A[4];
}
double TensorAlgebra2D::Tensor::trace(const ARRAY& A) {
  return A[0]+A[3]+A[4];
}
void TensorAlgebra2D::Tensor::prod(const ARRAY& A,const ARRAY& B,ARRAY& C) {
  C[0] = A[0]*B[0]+A[1]*B[2];
  C[1] = A[0]*B[1]+A[1]*B[3];
  C[2] = A[2]*B[0]+A[3]*B[2];
  C[3] = A[2]*B[1]+A[3]*B[3];
  C[4] = A[4]*B[4];
}
void TensorAlgebra2D::Tensor::log(const ARRAY& A,ARRAY& logA,ARRAY dLog[],
                                  ARRAY d2Log[][5],bool first,bool second) {
  unsigned int i,j,k;
  double vA[4],vLogA[4],dLogA[4][4],d2LogA[4][4][4];
  
  for (i=0; i < 4; i++) vA[i] = A[i];
  double norm = (vA[0]-1.0)*(vA[0]-1.0)+vA[1]*vA[1]
               +vA[2]*vA[2]+(vA[3]-1.0)*(vA[3]-1.0);
  if (norm < 0.04) {
    int test = logmat2_series(vA,vLogA,dLogA,d2LogA,first,second);
    if (!test) // OUCH!
      logmat2_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  }
  else
    logmat2_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  
  for (i=0; i < 4; i++) logA[i] = vLogA[i];
  logA[4] = std::log(A[4]);
  if (first) {
    for (i=0; i < 4; i++)
      for (j=0; j < 4; j++) dLog[i][j] = dLogA[i][j];
    dLog[0][4] = dLog[1][4] = dLog[2][4] = dLog[3][4] = 0.0e0;
    dLog[4][0] = dLog[4][1] = dLog[4][2] = dLog[4][3] = 0.0e0;
    dLog[4][4] = 1.0e0/A[4];
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
}

// operations
void TensorAlgebra2D::RightCauchyGreen(const ARRAY& F,ARRAY& C) {
  // C=F^t.F
  C[0] = F[0]*F[0]+F[2]*F[2];
  C[1] = F[0]*F[1]+F[2]*F[3];
  C[2] = F[1]*F[1]+F[3]*F[3];
  C[3] = F[4]*F[4];
}
void TensorAlgebra2D::RUDecomposition(const ARRAY& F,ARRAY& R,ARRAY& U) {
  // F=R.U
  double val1 = F[0]+F[3];
  double val2 = F[1]-F[2];
  double val = std::sqrt(val1*val1+val2*val2);
  val1 /= val;
  val2 /= val;
  R[0] =  val1; R[1] = val2;
  R[2] = -val2; R[3] = val1; R[4] = 1.e0;
  
  // U = R^-1.F
  U[0] = val1*F[0]-val2*F[2];
  U[1] = val1*F[1]-val2*F[3];
  U[2] = val2*F[1]+val1*F[3];
  U[3] = F[4];
}
void TensorAlgebra2D::CauchyToPK1(const ARRAY& sig,const ARRAY& F,ARRAY& P) {
  // P = sig.(J F^-t)
  ARRAY Fc(Tensor::MEMSIZE);
  Tensor::cofactor(F,Fc);
  P[0] = sig[0]*Fc[0]+sig[1]*Fc[2];
  P[1] = sig[0]*Fc[1]+sig[1]*Fc[3];
  P[2] = sig[1]*Fc[0]+sig[2]*Fc[2];
  P[3] = sig[1]*Fc[1]+sig[2]*Fc[3];
  P[4] = sig[3]*Fc[4];
}
void TensorAlgebra2D::PK1ToCauchy(const ARRAY& P,const ARRAY& F,ARRAY& sig) {
  // sig = (1/J) P.F^t
  double J = Tensor::determinant(F);
  double Jinv = 1.e0/J;
  sig[0] = Jinv*(P[0]*F[0]+P[1]*F[1]);
  sig[1] = Jinv*(P[2]*F[0]+P[3]*F[1]);
  sig[2] = Jinv*(P[2]*F[2]+P[3]*F[3]);
  sig[3] = Jinv*P[4]*F[4];
}
void TensorAlgebra2D::PK2ToKirchhoff(const ARRAY& S,const ARRAY& F,ARRAY& tau) {
  // tau = F.S.F^t
  tau = 0.e0;
  unsigned int i,j,k,l,ij,ik;
  for (i=0; i < DIMENSION; i++) {
    for (j=0; j <= i; j++) {
      ij = MAP1[i][j];
      for (k=0; k < DIMENSION; k++) {
        ik = MAP2[i][k];
        for (l=0; l < DIMENSION; l++)
          tau[ij] += F[ik]*S[MAP1[k][l]]*F[MAP2[j][l]];
      }
    }
  }
  tau[3] = F[4]*S[3]*F[4];
}
void TensorAlgebra2D::PK2ToPK1(const ARRAY& S,const ARRAY& F,ARRAY& P) {
  // P=F.S
  P[0] = F[0]*S[0]+F[1]*S[1];
  P[1] = F[0]*S[1]+F[1]*S[2];
  P[2] = F[2]*S[0]+F[3]*S[1];
  P[3] = F[2]*S[1]+F[3]*S[2];
  P[4] = F[4]*S[3];
}
void TensorAlgebra2D::MaterialToLagrangian(const MATRIX& M,const ARRAY& S,
                                           const ARRAY& F,MATRIX& T) {
  // T_iJkL=F_iI.M_IJKL.F_kK+S_JL 1_ik
  T = 0.e0;
  unsigned int i,j,k,l,m,n;
  for (i=0; i < DIMENSION; i++) {
    for (m=0; m < DIMENSION; m++) {
      unsigned int im = MAP2[i][m];
      for (j=0; j < DIMENSION; j++) {
        unsigned int ij = MAP2[i][j];
        unsigned int mj = MAP1[m][j];
        for (k=0; k < DIMENSION; k++)
          for (n=0; n < DIMENSION; n++) {
            unsigned int kn = MAP2[k][n];
            for (l=0; l < DIMENSION; l++)
              T[ij][MAP2[k][l]] += F[im]*M[mj][MAP1[n][l]]*F[kn];
          }
        T[ij][4] += F[im]*M[mj][3]*F[4];
        T[4][ij] += F[4]*M[3][mj]*F[im];
      }
    }
    for (j=0; j < DIMENSION; j++) {
      unsigned int ij = MAP2[i][j];
      for (l=0; l < DIMENSION; l++) T[ij][MAP2[i][l]] += S[MAP1[j][l]];
    }
  }
  T[4][4] = F[4]*M[3][3]*F[4]+S[3];
}
void TensorAlgebra2D::SpatialToLagrangian(const MATRIX& M,const ARRAY& sig,
                                          const ARRAY& F,MATRIX& T) {
  // compute PK2
  ARRAY Finv(Tensor::MEMSIZE);
  double J = Tensor::inverse(F,Finv);
  ARRAY S(SymTensor::MEMSIZE);
  S = 0.e0;
  unsigned int i,j,k,l;
  for (i=0; i < DIMENSION; i++) {
    for (j=0; j <= i; j++) {
      unsigned int ij = MAP1[i][j];
      for (k=0; k < DIMENSION; k++) {
        unsigned int ik = MAP2[i][k];
        for (l=0; l < DIMENSION; l++)
          S[ij] += J*Finv[ik]*sig[MAP1[k][l]]*Finv[MAP2[j][l]];
      }
    }
  }
  S[3] = Finv[4]*sig[3]*Finv[4];

  // T_iJkL=J F^-1_Jj.(M_ijkl+sig_jl 1_ik).F^-1_Ll
  T = 0.e0;
  unsigned int m,n;
  for (i=0; i < DIMENSION; i++) {
    for (m=0; m < DIMENSION; m++) {
      unsigned int im = MAP1[i][m];
      for (j=0; j < DIMENSION; j++) {
        unsigned int ij = MAP2[i][j];
        unsigned int jm = MAP2[j][m];
        for (k=0; k < DIMENSION; k++)
          for (n=0; n < DIMENSION; n++) {
            unsigned int kn = MAP1[k][n];
            for (l=0; l < DIMENSION; l++)
              T[ij][MAP2[k][l]] += J*Finv[jm]*M[im][kn]*Finv[MAP2[l][n]];
          }
        T[ij][4] += J*Finv[jm]*M[im][3]*Finv[4];
        T[4][ij] += J*Finv[4]*M[3][im]*Finv[jm];
      }
    }
    for (j=0; j < DIMENSION; j++) {
      unsigned int ij = MAP2[i][j];
      for (l=0; l < DIMENSION; l++) T[ij][MAP2[i][l]] += S[MAP1[j][l]];
    }
  }
  T[4][4] = J*Finv[4]*M[3][3]*Finv[4]+S[3];
}

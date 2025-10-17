/*
 *  $Id: TensorAlgebra3D.cpp 158 2015-01-10 21:14:05Z lstainier $
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
extern int logsym3_series(const double[],double[],double[][6],double[][6][6],bool,bool);
extern int logsym3_spectral(const double[],double[],double[][6],double[][6][6],bool,bool);
extern int logmat3_series(const double[],double[],double[][9],double[][9][9],bool,bool);
extern int logmat3_spectral(const double[],double[],double[][9],double[][9][9],bool,bool);

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for TensorAlgebra3D.
 */
static const int MAP1[3][3] = {{0,1,3},{1,2,4},{3,4,5}};
static const int MAP2[3][3] = {{0,1,2},{3,4,5},{6,7,8}};

// SymTensor operations
TensorAlgebra3D::ARRAY TensorAlgebra3D::SymTensor::identity() {
  ARRAY I(SymTensor::MEMSIZE);
  I[0] = I[2] = I[5] = 1.e0;
  I[1] = I[3] = I[4] = 0.e0;
  return I;
}
void TensorAlgebra3D::SymTensor::cofactor(const ARRAY& A,ARRAY& Ac) {
  Ac[0] =  A[2]*A[5]-A[4]*A[4];
  Ac[1] = -A[1]*A[5]+A[4]*A[3];
  Ac[2] =  A[0]*A[5]-A[3]*A[3];
  Ac[3] =  A[1]*A[4]-A[2]*A[3];
  Ac[4] = -A[0]*A[4]+A[3]*A[2];
  Ac[5] =  A[0]*A[2]-A[1]*A[1];
}
double TensorAlgebra3D::SymTensor::inverse(const ARRAY& A,ARRAY& Ainv) {
  double detA = determinant(A);
  double detInv = 1.e0/detA;
  Ainv[0] =  (A[2]*A[5]-A[4]*A[4])*detInv;
  Ainv[1] = -(A[1]*A[5]-A[4]*A[3])*detInv;
  Ainv[2] =  (A[0]*A[5]-A[3]*A[3])*detInv;
  Ainv[3] =  (A[1]*A[4]-A[2]*A[3])*detInv;
  Ainv[4] = -(A[0]*A[4]-A[3]*A[1])*detInv;
  Ainv[5] =  (A[0]*A[2]-A[1]*A[1])*detInv;
  return detA;
}
double TensorAlgebra3D::SymTensor::determinant(const ARRAY& A) {
  double detA = A[0]*(A[2]*A[5]-A[4]*A[4])
               -A[1]*(A[1]*A[5]-A[3]*A[4])
               +A[3]*(A[1]*A[4]-A[3]*A[2]);
  return detA;
}
double TensorAlgebra3D::SymTensor::trace(const ARRAY& A) {
  return A[0]+A[2]+A[5];
}
double TensorAlgebra3D::SymTensor::innerProd(const ARRAY& A,const ARRAY& B) {
  return A[0]*B[0]+A[2]*B[2]+A[5]*B[5]+(A[1]*B[1]+A[3]*B[3]+A[4]*B[4])*2;
}
void TensorAlgebra3D::SymTensor::log(const ARRAY& A,ARRAY& logA,ARRAY dLog[],
                                     ARRAY d2Log[][6],bool first,bool second) {
  unsigned int i,j,k;
  double vA[6],vLogA[6],dLogA[6][6],d2LogA[6][6][6];
  
  for (i=0; i < 6; i++) vA[i] = A[i];
  double norm = (vA[0]-1.0)*(vA[0]-1.0)
                +vA[1]*vA[1]+(vA[2]-1.0)*(vA[2]-1.0)
                +vA[3]*vA[3]+vA[4]*vA[4]+(vA[5]-1.0)*(vA[5]-1.0);
  if (norm < 0.04) {
    int test = logsym3_series(vA,vLogA,dLogA,d2LogA,first,second);
    if (!test)
      logsym3_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  }
  else
    logsym3_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  
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
}
void TensorAlgebra3D::SymTensor::covariant(const ARRAY& A,ARRAY& covA) {
  covA[0] = A[0];
  covA[1] = A[1]+A[1];
  covA[2] = A[2];
  covA[3] = A[3]+A[3];
  covA[4] = A[4]+A[4];
  covA[5] = A[5];
}
void TensorAlgebra3D::SymTensor::contravariant(const ARRAY& A,ARRAY& conA) {
  conA[0] =     A[0];
  conA[1] = 0.5*A[1];
  conA[2] =     A[2];
  conA[3] = 0.5*A[3];
  conA[4] = 0.5*A[4];
  conA[5] =     A[5];
}

// SymTensor4 operations
TensorAlgebra3D::MATRIX TensorAlgebra3D::SymTensor4::identity() {
  MATRIX I(SymTensor::MEMSIZE);
  I = 0.0e0;
  I[0][0] = I[2][2] = I[5][5] = 1.0e0;
  I[1][1] = I[3][3] = I[4][4] = 1.0e0;
  return I;
}
TensorAlgebra3D::MATRIX TensorAlgebra3D::SymTensor4::contravariantIdentity() {
  MATRIX I(SymTensor::MEMSIZE);
  I = 0.0e0;
  I[0][0] = I[2][2] = I[5][5] = 1.0e0;
  I[1][1] = I[3][3] = I[4][4] = 0.5e0;
  return I;
}
TensorAlgebra3D::MATRIX TensorAlgebra3D::SymTensor4::covariantIdentity() {
  MATRIX I(SymTensor::MEMSIZE);
  I = 0.e0;
  I[0][0] = I[2][2] = I[5][5] = 1.0e0;
  I[1][1] = I[3][3] = I[4][4] = 2.0e0;
  return I;
}
TensorAlgebra3D::MATRIX TensorAlgebra3D::SymTensor4::baseJ() {
  static const double ONE_THIRD = 1.e0/3.e0;
  static const double TWO_THIRD = 2.e0/3.e0;
  MATRIX J(SymTensor::MEMSIZE);
  J = 0.e0;
  J[0][0] =  TWO_THIRD; J[0][2] = -ONE_THIRD; J[0][5] = -ONE_THIRD;
  J[2][0] = -ONE_THIRD; J[2][2] =  TWO_THIRD; J[2][5] = -ONE_THIRD;
  J[5][0] = -ONE_THIRD; J[5][2] = -ONE_THIRD; J[5][5] =  TWO_THIRD;
  J[1][1] = J[3][3] = J[4][4] = 1.0e0;
  return J;
}
TensorAlgebra3D::MATRIX TensorAlgebra3D::SymTensor4::baseK() {
  static const double ONE_THIRD = 1.e0/3.e0;
  MATRIX K(SymTensor::MEMSIZE);
  K = 0.e0;
  K[0][0] = ONE_THIRD; K[0][2] = ONE_THIRD; K[0][5] = ONE_THIRD;
  K[2][0] = ONE_THIRD; K[2][2] = ONE_THIRD; K[2][5] = ONE_THIRD;
  K[5][0] = ONE_THIRD; K[5][2] = ONE_THIRD; K[5][5] = ONE_THIRD;
  return K;
}
void TensorAlgebra3D::SymTensor4::addIKJL(double coef,const ARRAY& A,MATRIX& M) {
  // M_ijkl += coef*(A_ik*A_jl+A_il*A_jk)
  unsigned int i,j,k,l,ij,kl; 
  for (i=0, ij=0; i < DIMENSION; i++)
    for (j=0; j <= i; j++, ij++)
      for (k=0, kl=0; k < DIMENSION; k++)
        for (l=0; l <= k; l++, kl++)
          M[ij][kl] += coef*(A[MAP1[i][k]]*A[MAP1[j][l]]
                            +A[MAP1[i][l]]*A[MAP1[j][k]]);
}
void TensorAlgebra3D::SymTensor4::outerProd(const ARRAY& A,MATRIX& M) {
  // M_ijkl = A_ij*A_kl
  for (unsigned int i=0; i < SymTensor::MEMSIZE; i++)
    for (unsigned int j=0; j < SymTensor::MEMSIZE; j++) M[i][j] = A[i]*A[j];
}
void TensorAlgebra3D::SymTensor4::outerProd2(const ARRAY& A,const ARRAY& B,MATRIX& M) {
  // M_ijkl = A_ij*B_kl+B_ij*A_kl
  for (unsigned int i=0; i < SymTensor::MEMSIZE; i++)
    for (unsigned int j=0; j < SymTensor::MEMSIZE; j++) 
      M[i][j] = A[i]*B[j]+B[i]*A[j];
}
void TensorAlgebra3D::SymTensor4::rightProd(const MATRIX& M,const ARRAY& A,ARRAY& B) {
  // B = M.A
  for (unsigned int i=0; i < SymTensor::MEMSIZE; i++) {
    double* p = M[i];
    B[i] = 0.e0;
    for (unsigned int j=0; j < SymTensor::MEMSIZE; j++) B[i] += p[j]*A[j];
  }
}

// Tensor operations
TensorAlgebra3D::ARRAY TensorAlgebra3D::Tensor::identity() {
  ARRAY I(Tensor::MEMSIZE);
  I[0] = I[4] = I[8] = 1.e0;
  I[1] = I[2] = I[3] = I[5] = I[6] = I[7] = 0.e0;
  return I;
}
void TensorAlgebra3D::Tensor::cofactor(const ARRAY& A,ARRAY& Ac) {
  Ac[0] =  A[4]*A[8]-A[5]*A[7];
  Ac[1] = -A[3]*A[8]+A[5]*A[6];
  Ac[2] =  A[3]*A[7]-A[4]*A[6];
  Ac[3] = -A[1]*A[8]+A[2]*A[7];
  Ac[4] =  A[0]*A[8]-A[2]*A[6];
  Ac[5] = -A[0]*A[7]+A[1]*A[6];
  Ac[6] =  A[1]*A[5]-A[2]*A[4];
  Ac[7] = -A[0]*A[5]+A[2]*A[3];
  Ac[8] =  A[0]*A[4]-A[1]*A[3];
}
double TensorAlgebra3D::Tensor::inverse(const ARRAY& A,ARRAY& Ainv) {
  double detA = determinant(A);
  double detInv = 1.e0/detA;
  Ainv[0] =  (A[4]*A[8]-A[5]*A[7])*detInv;
  Ainv[1] = -(A[1]*A[8]-A[2]*A[7])*detInv;
  Ainv[2] =  (A[1]*A[5]-A[2]*A[4])*detInv;
  Ainv[3] = -(A[3]*A[8]-A[5]*A[6])*detInv;
  Ainv[4] =  (A[0]*A[8]-A[2]*A[6])*detInv;
  Ainv[5] = -(A[0]*A[5]-A[2]*A[3])*detInv;
  Ainv[6] =  (A[3]*A[7]-A[4]*A[6])*detInv;
  Ainv[7] = -(A[0]*A[7]-A[1]*A[6])*detInv;
  Ainv[8] =  (A[0]*A[4]-A[1]*A[3])*detInv;
  return detA;
}
double TensorAlgebra3D::Tensor::determinant(const ARRAY& A) {
  double detA = A[0]*(A[4]*A[8]-A[5]*A[7])
               -A[1]*(A[3]*A[8]-A[5]*A[6])
               +A[2]*(A[3]*A[7]-A[4]*A[6]);
  return detA;
}
double TensorAlgebra3D::Tensor::trace(const ARRAY& A) {
  return A[0]+A[4]+A[8];
}
void TensorAlgebra3D::Tensor::prod(const ARRAY& A,const ARRAY& B,ARRAY& C) {
  C[0] = A[0]*B[0]+A[1]*B[3]+A[2]*B[6];
  C[1] = A[0]*B[1]+A[1]*B[4]+A[2]*B[7];
  C[2] = A[0]*B[2]+A[1]*B[5]+A[2]*B[8];
  C[3] = A[3]*B[0]+A[4]*B[3]+A[5]*B[6];
  C[4] = A[3]*B[1]+A[4]*B[4]+A[5]*B[7];
  C[5] = A[3]*B[2]+A[4]*B[5]+A[5]*B[8];
  C[6] = A[6]*B[0]+A[7]*B[3]+A[8]*B[6];
  C[7] = A[6]*B[1]+A[7]*B[4]+A[8]*B[7];
  C[8] = A[6]*B[2]+A[7]*B[5]+A[8]*B[8];
}
void TensorAlgebra3D::Tensor::log(const ARRAY& A,ARRAY& logA,ARRAY dLog[],
                                  ARRAY d2Log[][9],bool first,bool second) {
  unsigned int i,j,k;
  double vA[9],vLogA[9],dLogA[9][9],d2LogA[9][9][9];
  
  for (i=0; i < 9; i++) vA[i] = A[i];
  double norm = (vA[0]-1.0)*(vA[0]-1.0)+vA[1]*vA[1]+vA[2]*vA[2]
               +vA[3]*vA[3]+(vA[4]-1.0)*(vA[4]-1.0)+vA[5]*vA[5]
               +vA[6]*vA[6]+vA[7]*vA[7]+(vA[8]-1.0)*(vA[8]-1.0);
  if (norm < 0.04) {
    int test = logmat3_series(vA,vLogA,dLogA,d2LogA,first,second);
    if (!test) // OUCH!
      logmat3_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  }
  else
    logmat3_spectral(vA,vLogA,dLogA,d2LogA,first,second);
  
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
}

// operations
void TensorAlgebra3D::RightCauchyGreen(const ARRAY& F,ARRAY& C) {
  // C=F^t.F
  C[0] = F[0]*F[0]+F[3]*F[3]+F[6]*F[6];
  C[1] = F[0]*F[1]+F[3]*F[4]+F[6]*F[7];
  C[2] = F[1]*F[1]+F[4]*F[4]+F[7]*F[7];
  C[3] = F[0]*F[2]+F[3]*F[5]+F[6]*F[8];
  C[4] = F[1]*F[2]+F[4]*F[5]+F[7]*F[8];
  C[5] = F[2]*F[2]+F[5]*F[5]+F[8]*F[8];
}
void TensorAlgebra3D::RUDecomposition(const ARRAY& F,ARRAY& R,ARRAY& U) {
  // compute C and its invariants
  ARRAY C(SymTensor::MEMSIZE);
  RightCauchyGreen(F,C);
  ARRAY C2(SymTensor::MEMSIZE);
  C2[0] = C[0]*C[0]+C[1]*C[1]+C[3]*C[3];
  C2[1] = C[1]*C[0]+C[2]*C[1]+C[4]*C[3];
  C2[2] = C[1]*C[1]+C[2]*C[2]+C[4]*C[4];
  C2[3] = C[3]*C[0]+C[4]*C[1]+C[5]*C[3];
  C2[4] = C[3]*C[1]+C[4]*C[2]+C[5]*C[4];
  C2[5] = C[3]*C[3]+C[4]*C[4]+C[5]*C[5];
  double I1 = SymTensor::trace(C);
  double I2 = 0.5*(I1*I1-SymTensor::trace(C2));
  double I3 = SymTensor::determinant(C);
  
  // compute principal stretches
  static const double PI = 4*std::atan(1.e0);
  static const double THIRD = 1.e0/3.e0;
  double x1,x2,x3;
  double b = I2-I1*I1*THIRD;
  double c = -2.*I1*I1*I1/27.+I1*I2*THIRD-I3;
  if (std::fabs(b) < 1.e-06*I2) {
    if (c >= 0.0e0)
      x1 = x2 = x3 = -std::pow(c,THIRD);
    else
      x1 = x2 = x3 = std::pow(-c,THIRD);
  }
  else {
    double d = 2*std::sqrt(-b*THIRD);
    double e = 3*c/(d*b);
    double val = 1.e0-e*e;
    if (std::fabs(val) < 1.e-06) val = 0.0e0;
    double t = std::atan2(std::sqrt(val),e)*THIRD;
    x1 = d*std::cos(t);
    x2 = d*std::cos(t+2*THIRD*PI);
    x3 = d*std::cos(t+4*THIRD*PI);
  }
  double l1 = std::sqrt(x1+I1*THIRD);
  double l2 = std::sqrt(x2+I1*THIRD);
  double l3 = std::sqrt(x3+I1*THIRD);
  
  // compute U and U^-1
  ARRAY Uinv(SymTensor::MEMSIZE);
  double i1 = l1+l2+l3;
  double i2 = l1*l2+l1*l3+l2*l3;
  double i3 = l1*l2*l3;
  double D = i1*i2-i3;
  U = (1.e0/D)*(-C2+(i1*i1-i2)*C+(i1*i3)*SymTensor::identity());
  Uinv = (1.e0/i3)*(C-i1*U+i2*SymTensor::identity());
  
  // compute R
  R[0] = F[0]*Uinv[0]+F[1]*Uinv[1]+F[2]*Uinv[3];
  R[1] = F[0]*Uinv[1]+F[1]*Uinv[2]+F[2]*Uinv[4];
  R[2] = F[0]*Uinv[3]+F[1]*Uinv[4]+F[2]*Uinv[5];
  R[3] = F[3]*Uinv[0]+F[4]*Uinv[1]+F[5]*Uinv[3];
  R[4] = F[3]*Uinv[1]+F[4]*Uinv[2]+F[5]*Uinv[4];
  R[5] = F[3]*Uinv[3]+F[4]*Uinv[4]+F[5]*Uinv[5];
  R[6] = F[6]*Uinv[0]+F[7]*Uinv[1]+F[8]*Uinv[3];
  R[7] = F[6]*Uinv[1]+F[7]*Uinv[2]+F[8]*Uinv[4];
  R[8] = F[6]*Uinv[3]+F[7]*Uinv[4]+F[8]*Uinv[5];
}
void TensorAlgebra3D::CauchyToPK1(const ARRAY& sig,const ARRAY& F,ARRAY& P) {
  // P = sig.(J F^-t)
  ARRAY Fc(Tensor::MEMSIZE);
  Tensor::cofactor(F,Fc);
  P[0] = sig[0]*Fc[0]+sig[1]*Fc[3]+sig[3]*Fc[6];
  P[1] = sig[0]*Fc[1]+sig[1]*Fc[4]+sig[3]*Fc[7];
  P[2] = sig[0]*Fc[2]+sig[1]*Fc[5]+sig[3]*Fc[8];
  P[3] = sig[1]*Fc[0]+sig[2]*Fc[3]+sig[4]*Fc[6];
  P[4] = sig[1]*Fc[1]+sig[2]*Fc[4]+sig[4]*Fc[7];
  P[5] = sig[1]*Fc[2]+sig[2]*Fc[5]+sig[4]*Fc[8];
  P[6] = sig[3]*Fc[0]+sig[4]*Fc[3]+sig[5]*Fc[6];
  P[7] = sig[3]*Fc[1]+sig[4]*Fc[4]+sig[5]*Fc[7];
  P[8] = sig[3]*Fc[2]+sig[4]*Fc[5]+sig[5]*Fc[8];
}
void TensorAlgebra3D::PK1ToCauchy(const ARRAY& P,const ARRAY& F,ARRAY& sig) {
  // sig = (1/J) P.F^t
  double J = Tensor::determinant(F);
  double Jinv = 1.e0/J;
  sig[0] = Jinv*(P[0]*F[0]+P[1]*F[1]+P[2]*F[2]);
  sig[1] = Jinv*(P[3]*F[0]+P[4]*F[1]+P[5]*F[2]);
  sig[2] = Jinv*(P[3]*F[3]+P[4]*F[4]+P[5]*F[5]);
  sig[3] = Jinv*(P[6]*F[0]+P[7]*F[1]+P[8]*F[2]);
  sig[4] = Jinv*(P[6]*F[3]+P[7]*F[4]+P[8]*F[5]);
  sig[5] = Jinv*(P[6]*F[6]+P[7]*F[7]+P[8]*F[8]);
}
void TensorAlgebra3D::PK2ToKirchhoff(const ARRAY& S,const ARRAY& F,ARRAY& tau) {
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
}
void TensorAlgebra3D::PK2ToPK1(const ARRAY& S,const ARRAY& F,ARRAY& P) {
  // P=F.S
  P[0] = F[0]*S[0]+F[1]*S[1]+F[2]*S[3];
  P[1] = F[0]*S[1]+F[1]*S[2]+F[2]*S[4];
  P[2] = F[0]*S[3]+F[1]*S[4]+F[2]*S[5];
  P[3] = F[3]*S[0]+F[4]*S[1]+F[5]*S[3];
  P[4] = F[3]*S[1]+F[4]*S[2]+F[5]*S[4];
  P[5] = F[3]*S[3]+F[4]*S[4]+F[5]*S[5];
  P[6] = F[6]*S[0]+F[7]*S[1]+F[8]*S[3];
  P[7] = F[6]*S[1]+F[7]*S[2]+F[8]*S[4];
  P[8] = F[6]*S[3]+F[7]*S[4]+F[8]*S[5];
}
void TensorAlgebra3D::MaterialToLagrangian(const MATRIX& M,const ARRAY& S,
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
      }
    }
    for (j=0; j < DIMENSION; j++) {
      unsigned int ij = MAP2[i][j];
      for (l=0; l < DIMENSION; l++) T[ij][MAP2[i][l]] += S[MAP1[j][l]];
    }
  }
}
void TensorAlgebra3D::SpatialToLagrangian(const MATRIX& M,const ARRAY& sig,
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
      }
    }
    for (j=0; j < DIMENSION; j++) {
      unsigned int ij = MAP2[i][j];
      for (l=0; l < DIMENSION; l++) T[ij][MAP2[i][l]] += S[MAP1[j][l]];
    }
  }
}

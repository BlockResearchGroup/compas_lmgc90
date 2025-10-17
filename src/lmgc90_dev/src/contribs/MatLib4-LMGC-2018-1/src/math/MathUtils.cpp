/*
 *  $Id: MathUtils.cpp 143 2014-04-18 07:12:34Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "MathUtils.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>

/**
 * Vector functions.
 */

// addition
void MATLIB_NAMESPACE addvec(const double a[],const double b[],
                             double c[],unsigned int n) {
  for (unsigned int i=0; i < n; i++, a++, b++, c++) (*c) = (*a)+(*b);
}

// multiplication by a scalar
void MATLIB_NAMESPACE mulvec(double a,const double b[],double c[],unsigned int n) {
  for (unsigned int i=0; i < n; i++, b++, c++) (*c) = a*(*b);
}

// norm LInf
double MATLIB_NAMESPACE nrmvec0(const double a[],unsigned int n) {
  double norm = 0.e0;
  for (unsigned int i=0; i < n; i++, a++) {
    double test = std::fabs(*a);
    norm = (test > norm) ? test:norm;
  }
  return norm;
}

// norm L1
double MATLIB_NAMESPACE nrmvec1(const double a[],unsigned int n) {
  double norm = 0.e0;
  for (unsigned int i=0; i < n; i++, a++) norm += std::fabs(*a);
  return norm;
}

// norm L2
double MATLIB_NAMESPACE nrmvec2(const double a[],unsigned int n) {
  double norm = 0.e0;
  for (unsigned int i=0; i < n; i++, a++) norm += (*a)*(*a);
  return std::sqrt(norm);
}


/**
 * 2x2 matrix functions
 */

// matrix multiplication
void MATLIB_NAMESPACE mulmat2(const double *a,const double *b,double *c) {
  c[0] = a[0]*b[0]+a[1]*b[2];
  c[1] = a[0]*b[1]+a[1]*b[3];
  c[2] = a[2]*b[0]+a[3]*b[2];
  c[3] = a[2]*b[1]+a[3]*b[3];
}

// matrix determinant
double MATLIB_NAMESPACE detmat2(const double *a) {
  return a[0]*a[3]-a[1]*a[2];
}

// matrix inverse
double MATLIB_NAMESPACE invmat2(const double *a,double *ainv,double prec) {
  double det = detmat2(a);
  if (std::fabs(det) < prec) return 0.e0;
  double detinv = 1.e0/det;
  ainv[0] =  a[3]*detinv;
  ainv[1] = -a[1]*detinv;
  ainv[2] = -a[2]*detinv;
  ainv[3] =  a[0]*detinv;
  return det;
}

// matrix transposed inverse
double MATLIB_NAMESPACE tnvmat2(const double *a,double *ainv,double prec) {
  double det = detmat2(a);
  if (std::fabs(det) < prec) return 0.e0;
  double detinv = 1.e0/det;
  ainv[0] =  a[3]*detinv;
  ainv[1] = -a[2]*detinv;
  ainv[2] = -a[1]*detinv;
  ainv[3] =  a[0]*detinv;
  return det;
}

// symmetric matrix determinant
double MATLIB_NAMESPACE detsym2(const double *a) {
  return a[0]*a[2]-a[1]*a[1];
}

// symmetric matrix inverse
double MATLIB_NAMESPACE invsym2(const double *a,double *ainv,double prec) {
  double det = detsym2(a);
  if (std::fabs(det) < prec) return 0.e0;
  double detinv = 1.e0/det;
  ainv[0] =  a[2]*detinv;
  ainv[1] = -a[1]*detinv;
  ainv[2] =  a[0]*detinv;
  return det;
}


/**
 * 3x3 matrix functions
 */

// matrix multiplication
void MATLIB_NAMESPACE mulmat3(const double *a,const double *b,double *c) {
  c[0] = a[0]*b[0]+a[1]*b[3]+a[2]*b[6];
  c[1] = a[0]*b[1]+a[1]*b[4]+a[2]*b[7];
  c[2] = a[0]*b[2]+a[1]*b[5]+a[2]*b[8];
  c[3] = a[3]*b[0]+a[4]*b[3]+a[5]*b[6];
  c[4] = a[3]*b[1]+a[4]*b[4]+a[5]*b[7];
  c[5] = a[3]*b[2]+a[4]*b[5]+a[5]*b[8];
  c[6] = a[6]*b[0]+a[7]*b[3]+a[8]*b[6];
  c[7] = a[6]*b[1]+a[7]*b[4]+a[8]*b[7];
  c[8] = a[6]*b[2]+a[7]*b[5]+a[8]*b[8];
}

// matrix determinant
double MATLIB_NAMESPACE detmat3(const double *a) {
  return a[0]*(a[4]*a[8]-a[5]*a[7])
        -a[1]*(a[3]*a[8]-a[5]*a[6])
        +a[2]*(a[3]*a[7]-a[4]*a[6]);
}

// matrix inverse
double MATLIB_NAMESPACE invmat3(const double *a,double *ainv,double prec) {
  double det = detmat3(a);
  if (std::fabs(det) < prec) return 0.e0;
  double detinv = 1.e0/det;
  ainv[0] =  (a[4]*a[8]-a[5]*a[7])*detinv;
  ainv[1] = -(a[1]*a[8]-a[2]*a[7])*detinv;
  ainv[2] =  (a[1]*a[5]-a[2]*a[4])*detinv;
  ainv[3] = -(a[3]*a[8]-a[5]*a[6])*detinv;
  ainv[4] =  (a[0]*a[8]-a[2]*a[6])*detinv;
  ainv[5] = -(a[0]*a[5]-a[2]*a[3])*detinv;
  ainv[6] =  (a[3]*a[7]-a[4]*a[6])*detinv;
  ainv[7] = -(a[0]*a[7]-a[1]*a[6])*detinv;
  ainv[8] =  (a[0]*a[4]-a[1]*a[3])*detinv;
  return det;
}

// matrix transposed inverse
double MATLIB_NAMESPACE tnvmat3(const double *a,double *ainv,double prec) {
  double det = detmat3(a);
  if (std::fabs(det) < prec) return 0.e0;
  double detinv = 1.e0/det;
  ainv[0] =  (a[4]*a[8]-a[5]*a[7])*detinv;
  ainv[1] = -(a[3]*a[8]-a[5]*a[6])*detinv;
  ainv[2] =  (a[3]*a[7]-a[4]*a[6])*detinv;
  ainv[3] = -(a[1]*a[8]-a[2]*a[7])*detinv;
  ainv[4] =  (a[0]*a[8]-a[2]*a[6])*detinv;
  ainv[5] = -(a[0]*a[7]-a[1]*a[6])*detinv;
  ainv[6] =  (a[1]*a[5]-a[2]*a[4])*detinv;
  ainv[7] = -(a[0]*a[5]-a[2]*a[3])*detinv;
  ainv[8] =  (a[0]*a[4]-a[1]*a[3])*detinv;
  return det;
}

// symmetric matrix determinant
double MATLIB_NAMESPACE detsym3(const double *a) {
  return a[0]*(a[2]*a[5]-a[4]*a[4])
        -a[1]*(a[1]*a[5]-a[3]*a[4])
        +a[3]*(a[1]*a[4]-a[2]*a[3]);
}

// matrix inverse
double MATLIB_NAMESPACE invsym3(const double *a,double *ainv,double prec) {
  double det = detsym3(a);
  if (std::fabs(det) < prec) return 0.e0;
  double detinv = 1.e0/det;
  ainv[0] =  (a[2]*a[5]-a[4]*a[4])*detinv;
  ainv[1] = -(a[1]*a[5]-a[3]*a[4])*detinv;
  ainv[2] =  (a[0]*a[5]-a[3]*a[3])*detinv;
  ainv[3] =  (a[1]*a[4]-a[2]*a[3])*detinv;
  ainv[4] = -(a[0]*a[4]-a[1]*a[3])*detinv;
  ainv[5] =  (a[0]*a[2]-a[1]*a[1])*detinv;
  return det;
}

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

// templated functions
template<>
double determinant<1>(const ShortArray& a) {
  return a[0]*a[1]*a[2];
}
template<>
double determinant<2>(const ShortArray& a) {
  return (a[0]*a[3]-a[1]*a[2])*a[4];
}
template<>
double determinant<3>(const ShortArray& a) {
  return a[0]*(a[4]*a[8]-a[5]*a[7])
        -a[1]*(a[3]*a[8]-a[5]*a[6])
        +a[2]*(a[3]*a[7]-a[4]*a[6]);
}

template<>
void eigenSym<1>(const ShortArray& a,double v[]) {
  v[0] = a[0];
  v[1] = a[1];
  v[2] = a[2];
}
template<>
void eigenSym<2>(const ShortArray& a,double v[]) {
  double tr = a[0]+a[2];
  double det = a[0]*a[2]-a[1]*a[1];
  double val = std::sqrt(tr*tr-4*det);
  v[0] = 0.5*(tr-val);
  v[1] = 0.5*(tr+val);
  v[2] = a[3];
}
template<>
void eigenSym<3>(const ShortArray& a,double v[]) {
  static const double PI = 4*std::atan(1.0);
  double val = a[1]*a[1]+a[3]*a[3]+a[4];
  // diagonal matrix
  if (val < 1.e-16) {
    v[0] = a[0];
    v[1] = a[2];
    v[2] = a[5];
  }
  // general symmetric matrix
  else {
    double q = (a[0]+a[2]+a[5])/3;
    ShortArray b(a);
    b[0] -= q;
    b[2] -= q;
    b[5] -= q;
    double p = std::sqrt((b[0]*b[0]+b[2]*b[2]+b[5]*b[5]+2*val)/6);
    b /= p;
    double r = 0.5*detsym3(&b[0]);
    double phi;
    if (r <= -1.0)
      phi = PI/3;
    else if (r >= 1.0)
      phi = 0.0;
    else
      phi = std::acos(r)/3;
    v[0] = q+2*p*std::cos(phi);
    v[2] = q+2*p*std::cos(phi+2*PI/3);
    v[1] = 3*q-v[0]-v[2];
  }
}

template<>
double innerProd<1>(const ShortArray& a,const ShortArray& b) {
  return a[0]*b[0]+a[1]*b[1]+a[2]*b[2];
}
template<>
double innerProd<2>(const ShortArray& a,const ShortArray& b) {
  return a[0]*b[0]+a[1]*b[1]
        +a[2]*b[2]+a[3]*b[3]
        +a[4]*b[4];
}
template<>
double innerProd<3>(const ShortArray& a,const ShortArray& b) {
  return a[0]*b[0]+a[1]*b[1]+a[2]*b[2]
        +a[3]*b[3]+a[4]*b[4]+a[5]*b[5]
        +a[6]*b[6]+a[7]*b[7]+a[8]*b[8];
}

template <>
double invert<1>(const ShortArray& a,ShortArray& aInv) {
  double d = a[0]*a[1]*a[2];
  aInv[0] = 1.e0/a[0];
  aInv[1] = 1.e0/a[1];
  aInv[2] = 1.e0/a[2];
  return d;
}
template <>
double invert<2>(const ShortArray& a,ShortArray& aInv) {
  double d = (a[0]*a[3]-a[1]*a[2]);
  double di = 1.e0/d;
  aInv[0] =  a[3]*di;
  aInv[1] = -a[1]*di;
  aInv[2] = -a[2]*di;
  aInv[3] =  a[0]*di;
  aInv[4] = 1.e0/a[4];
  return d*a[4];
}
template <>
double invert<3>(const ShortArray& a,ShortArray& aInv) {
  double d = a[0]*(a[4]*a[8]-a[5]*a[7])
            -a[1]*(a[3]*a[8]-a[5]*a[6])
            +a[2]*(a[3]*a[7]-a[4]*a[6]);
  double di = 1.e0/d;
  aInv[0] =  (a[4]*a[8]-a[5]*a[7])*di;
  aInv[1] = -(a[1]*a[8]-a[2]*a[7])*di;
  aInv[2] =  (a[1]*a[5]-a[2]*a[4])*di;
  aInv[3] = -(a[3]*a[8]-a[5]*a[6])*di;
  aInv[4] =  (a[0]*a[8]-a[2]*a[6])*di;
  aInv[5] = -(a[0]*a[5]-a[2]*a[3])*di;
  aInv[6] =  (a[3]*a[7]-a[4]*a[6])*di;
  aInv[7] = -(a[0]*a[7]-a[1]*a[6])*di;
  aInv[8] =  (a[0]*a[4]-a[1]*a[3])*di;
  return d;
}

template <>
void transpose<1>(const ShortArray& a,ShortArray& aT) {
  aT[0] = a[0]; aT[1] = a[1]; aT[2] = a[2];
}
template <>
void transpose<2>(const ShortArray& a,ShortArray& aT) {
  aT[0] = a[0]; aT[1] = a[2];
  aT[2] = a[1]; aT[3] = a[3];
  aT[4] = a[4];
}
template <>
void transpose<3>(const ShortArray& a,ShortArray& aT) {
  aT[0] = a[0]; aT[1] = a[3]; aT[2] = a[6];
  aT[3] = a[1]; aT[4] = a[4]; aT[5] = a[7];
  aT[6] = a[2]; aT[7] = a[5]; aT[8] = a[8];
}

template<>
ShortArray identSym<1>() {
  ShortArray I(3);
  I[0] = 1.0; I[1] = 1.0; I[2] = 1.0;
  return I;
}
template<>
ShortArray identSym<2>() {
  ShortArray I(4);
  I[0] = 1.0;
  I[1] = 0.0; I[2] = 1.0;
  I[3] = 1.0;
  return I;
}
template<>
ShortArray identSym<3>() {
  ShortArray I(6);
  I[0] = 1.0;
  I[1] = 0.0; I[2] = 1.0;
  I[3] = 0.0; I[4] = 0.0; I[5] = 1.0;
  return I;
}

template<>
ShortArray identity<1>() {
  ShortArray I(3);
  I[0] = 1.0; I[1] = 1.0; I[2] = 1.0;
  return I;
}
template<>
ShortArray identity<2>() {
  ShortArray I(5);
  I[0] = 1.0; I[1] = 0.0;
  I[2] = 0.0; I[3] = 1.0;
  I[4] = 1.0;
  return I;
}
template<>
ShortArray identity<3>() {
  ShortArray I(9);
  I[0] = 1.0; I[1] = 0.0; I[2] = 0.0;
  I[3] = 0.0; I[4] = 1.0; I[5] = 0.0;
  I[6] = 0.0; I[7] = 0.0; I[8] = 1.0;
  return I;
}

template<>
double trSym<1>(const ShortArray& a) {
  return a[0]+a[1]+a[2];
}
template<>
double trSym<2>(const ShortArray& a) {
  return a[0]+a[2]+a[3];
}
template<>
double trSym<3>(const ShortArray& a) {
  return a[0]+a[2]+a[5];
}

template<>
double trace<1>(const ShortArray& a) {
  return a[0]+a[1]+a[2];
}
template<>
double trace<2>(const ShortArray& a) {
  return a[0]+a[3]+a[4];
}
template<>
double trace<3>(const ShortArray& a) {
  return a[0]+a[4]+a[8];
}

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

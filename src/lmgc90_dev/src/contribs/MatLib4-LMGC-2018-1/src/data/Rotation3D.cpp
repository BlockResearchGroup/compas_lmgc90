/*
 *  $Id: Rotation3D.cpp 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "Rotation3D.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>


/*
 * Methods for class Rotation3D.
 */

// constructor (from Euler angles)
Rotation3D::Rotation3D(double a1,double a2,double a3,Type type) {
  
  static const double HALF_PI = 2.0*std::atan(1.0e0);

  // transform angles
  double coef = HALF_PI/90.;
  double psi,theta,phi;
  if (type == BUNGE) {
    psi = coef*a1-HALF_PI;
    theta = coef*a2;
    phi = HALF_PI-coef*a3;
  }
  else { // symmetric Euler angles
    psi = coef*a1;
    theta = coef*a2;
    phi = coef*a3;
  }
  
  // compute rotation matrix
  ShortSqrMatrix R(3);
  double c1 = std::cos(psi);
  double s1 = std::sin(psi);
  double c2 = std::cos(theta);
  double s2 = std::sin(theta);
  double c3 = std::cos(phi);
  double s3 = std::sin(phi);
  R[0][0] = -s1*s3-c1*c3*c2;
  R[0][1] =  s1*c3-c1*s3*c2;
  R[0][2] =  c1*s2;
  R[1][0] =  c1*s3-s1*c3*c2;
  R[1][1] = -c1*c3-s1*s3*c2;
  R[1][2] =  s1*s2;
  R[2][0] =  c3*s2;
  R[2][1] =  s3*s2;
  R[2][2] =  c2;
  
  // compute Euler parameters
  ShortArray e(4);
  euler(R,e);
  
  // transform to CRV
  coef = 4.e0/(1.e0+e[0]);
  c.resize(3);
  c0   = coef*e[0];
  c[0] = coef*e[1];
  c[1] = coef*e[2];
  c[2] = coef*e[3];
}

// constructor (from Euler parameters)
Rotation3D::Rotation3D(const ShortArray& e) {
  // transform to CRV
  double coef = 4.e0/(1.e0+e[0]);
  c.resize(3);
  c0   = coef*e[0];
  c[0] = coef*e[1];
  c[1] = coef*e[2];
  c[2] = coef*e[3];
}

// constructor (from rotation matrix)
Rotation3D::Rotation3D(const ShortSqrMatrix& R) {
  // compute Euler parameters
  ShortArray e(4);
  euler(R,e);
  
  // transform to CRV
  double coef = 4.e0/(1.e0+e[0]);
  c.resize(3);
  c0   = coef*e[0];
  c[0] = coef*e[1];
  c[1] = coef*e[2];
  c[2] = coef*e[3];
}

// copy constructor
Rotation3D::Rotation3D(const Rotation3D& src) {
  c0 = src.c0;
  c.resize(3);
  c  = src.c;
}

// export to Euler angles
void Rotation3D::toEulerAngles(double& a1,double& a2,double& a3,Type type) const {
  
  // compute Rodrigues parameters
  ShortArray b(3);
  b = c/c0;
  
  // compute Bunge angles
  double sum = std::atan(b[2]);
  double dif = std::atan2(b[1],b[0]);
  double phi1 = sum+dif;
  double theta = 2.0*std::atan2(b[0]*std::cos(sum),std::cos(dif));
  double phi2 = sum-dif;
  
  // angles in degrees
  static const double HALF_PI = 2.0*std::atan(1.0e0);
  double coef = 90./HALF_PI;
  if (type == BUNGE) {
    a1 = coef*phi1;
    a2 = coef*theta;
    a3 = coef*phi2;
  }
  else { // symmetric Euler angles
    a1 = coef*(phi1-HALF_PI);
    a2 = coef*theta;
    a3 = coef*(HALF_PI-phi2);
  }
}

// export to matrix
void Rotation3D::toMatrix(ShortSqrMatrix& R) const {
  R.resize(3);
  ShortSqrMatrix cc(3),C(3);
  ShortMatrix::outerProd(c,c,cc);
  spin(c,C);
  double coef = 4.-c0;
  coef = 1.e0/(coef*coef);
  R = coef*((c0*c0+8*c0-16.)*ShortSqrMatrix::identity(3)
            +2.*cc+(2*c0)*C);
  return;
}

// export to tensor
void Rotation3D::toTensor(ShortArray& r) const {
  r.resize(9);
  ShortSqrMatrix R(3);
  this->toMatrix(R);
  unsigned int i,j,ij;
  for (i=0, ij=0; i < 3; i++)
    for (j=0; j < 3; j++, ij++) r[ij] = R[i][j];
  return;
}

// from Euler parameters to CRV representation
void Rotation3D::eul2crv(const ShortArray& e,ShortArray& c) {
  // transform to CRV
  double coef = 4.e0/(1.e0+e[0]);
  c.resize(4);
  c = coef*e;
}

// from matrix representation to Euler parameters
void Rotation3D::euler(const ShortSqrMatrix& R,ShortArray& e) {
  
  // find index with max value (for numerical stability)
  double val[4];
  val[0] = 1.e0+R[0][0]+R[1][1]+R[2][2];
  val[1] = 1.e0+R[0][0]-R[1][1]-R[2][2];
  val[2] = 1.e0-R[0][0]+R[1][1]-R[2][2];
  val[3] = 1.e0-R[0][0]-R[1][1]+R[2][2];
  int iMax=0;
  double vMax=val[0];
  for (int i=1; i < 4; i++) {
    if (val[i] > vMax) {
      iMax = i;
      vMax = val[i];
    }
  }
  
  // compute Euler parameters
  double coef;
  switch (iMax) {
    case 0:
      e[0] = 0.5*std::sqrt(vMax);
      coef = 0.25/e[0];
      e[1] = coef*(R[2][1]-R[1][2]);
      e[2] = coef*(R[0][2]-R[2][0]);
      e[3] = coef*(R[1][0]-R[0][1]);
      break;
    case 1:
      e[1] = 0.5*std::sqrt(vMax);
      coef = 0.25/e[1];
      e[0] = coef*(R[2][1]-R[1][2]);
      e[2] = coef*(R[0][1]+R[1][0]);
      e[3] = coef*(R[0][2]+R[2][0]);
      break;
    case 2:
      e[2] = 0.5*std::sqrt(vMax);
      coef = 0.25/e[2];
      e[0] = coef*(R[0][2]-R[2][0]);
      e[1] = coef*(R[0][1]+R[1][0]);
      e[3] = coef*(R[1][2]+R[2][1]);
      break;
    case 3:
      e[3] = 0.5*std::sqrt(vMax);
      coef = 0.25/e[3];
      e[0] = coef*(R[1][0]-R[0][1]);
      e[1] = coef*(R[0][2]+R[2][0]);
      e[2] = coef*(R[1][2]+R[2][1]);
      break;
  }
  
  return;
}

// from vector to matrix representation
void Rotation3D::spin(const ShortArray& r,ShortSqrMatrix& R) {
  R[0][0] =  0.e0; R[0][1] = -r[2]; R[0][2] =  r[1];
  R[1][0] =  r[2]; R[1][1] =  0.e0; R[1][2] = -r[0];
  R[2][0] = -r[1]; R[2][1] =  r[0]; R[2][2] =  0.e0;
}

/*
 *  $Id: Rotation2D.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "Rotation2D.h"

// std C library
#include <cmath>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class Rotation2D.
 */

// constructor (from angle)
Rotation2D::Rotation2D(double th) {
  theta = th;
}

// constructor (from rotation matrix)
Rotation2D::Rotation2D(const ShortSqrMatrix& R) {
  theta = std::atan2(R[1][0],R[0][0]);
}

// copy constructor
Rotation2D::Rotation2D(const Rotation2D& src) {
  theta = src.theta;
}

// export to matrix
void Rotation2D::toMatrix(ShortSqrMatrix& R) const {
  R.resize(2);
  double cs = std::cos(theta);
  double sn = std::sin(theta);
  R[0][0] = cs; R[0][1] = -sn;
  R[1][0] = sn; R[1][1] =  cs;
  return;
}
  
// export to tensor
void Rotation2D::toTensor(ShortArray& R) const {
  R.resize(5);
  double cs = std::cos(theta);
  double sn = std::sin(theta);
  R[0] = cs; R[1] = -sn;
  R[2] = sn; R[3] =  cs;
  R[4] = 1.e0;
  return;
}

/*
 *  $Id: Tensor4_3D.cpp 133 2013-07-22 18:47:44Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "Tensor4_3D.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for Tensor4_3D
 */

// specific operation
void Tensor4_3D::addIJKL(double coef,const Tensor3D& A) {
  // T_ijkl += coef*A_il*A_kj
  unsigned int i,j,k,l,ij,kl,i0,k0;
  for (i=0, ij=0, i0=0; i < 3; i++, i0+=3)
    for (j=0; j < 3; j++, ij++)
      for (k=0, kl=0, k0=0; k < 3; k++, k0+=3)
        for (l=0; l < 3; l++, kl++)
          (*this)[ij][kl] += coef*A[i0+l]*A[k0+j];
}
void Tensor4_3D::addIJKL(double coef,const Tensor3D& A,const Tensor3D& B) {
  // T_ijkl += coef*A_il*B_kj
  unsigned int i,j,k,l,ij,kl,i0,k0;
  for (i=0, ij=0, i0=0; i < 3; i++, i0+=3)
    for (j=0; j < 3; j++, ij++)
      for (k=0, kl=0, k0=0; k < 3; k++, k0+=3)
        for (l=0; l < 3; l++, kl++)
          (*this)[ij][kl] += coef*A[i0+l]*B[k0+j];
}

// identity tensor
Tensor4_3D Tensor4_3D::identity() {
  Tensor4_3D I;
  I = 0.0e0;
  I[0][0] = I[4][4] = I[8][8] = 1.0e0;
  I[1][1] = I[2][2] = I[3][3] = 1.0e0;
  I[5][5] = I[6][6] = I[7][7] = 1.0e0;
  return I;
}

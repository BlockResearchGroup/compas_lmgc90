/*
 *  $Id: Tensor4_2D.cpp 133 2013-07-22 18:47:44Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "Tensor4_2D.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for Tensor4_2D
 */

// specific operation
void Tensor4_2D::addIJKL(double coef,const Tensor2D& A) {
  // T_ijkl += coef*A_il*A_kj
  unsigned int i,j,k,l,ij,kl,i0,k0;
  for (i=0, ij=0, i0=0; i < 2; i++, i0+=2)
    for (j=0; j < 2; j++, ij++)
      for (k=0, kl=0, k0=0; k < 2; k++, k0+=2)
        for (l=0; l < 2; l++, kl++)
          (*this)[ij][kl] += coef*A[i0+l]*A[k0+j];
  (*this)[4][4] += coef*A[4]*A[4];
}
void Tensor4_2D::addIJKL(double coef,const Tensor2D& A,const Tensor2D& B) {
  // T_ijkl += coef*A_il*B_kj
  unsigned int i,j,k,l,ij,kl,i0,k0;
  for (i=0, ij=0, i0=0; i < 2; i++, i0+=2)
    for (j=0; j < 2; j++, ij++)
      for (k=0, kl=0, k0=0; k < 2; k++, k0+=2)
        for (l=0; l < 2; l++, kl++)
          (*this)[ij][kl] += coef*A[i0+l]*B[k0+j];
  (*this)[4][4] += coef*A[4]+B[4];
}

// identity tensor
Tensor4_2D Tensor4_2D::identity() {
  Tensor4_2D I;
  I = 0.0e0;
  I[0][0] = I[3][3] = I[4][4] = 1.0e0;
  I[1][1] = I[2][2] = 1.0e0;
  return I;
}

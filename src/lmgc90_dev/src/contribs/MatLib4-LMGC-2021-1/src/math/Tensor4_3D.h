/*
 *  $Id: Tensor4_3D.h 133 2013-07-22 18:47:44Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_TENSOR4_3D_H
#define ZORGLIB_MATH_TENSOR4_3D_H

// config
#include <matlib_macros.h>

// local
#include <data/ShortSqrMatrix.h>
#include <math/Tensor3D.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class encapsulating 3D 4th-order tensor objects.
 * It inherits methods and internal structure from ShortSqrMatrix.
 */
class Tensor4_3D : virtual public ShortSqrMatrix {
  
 public:
  
  // default constructor
  Tensor4_3D() : ShortMatrix(Tensor3D::MEMSIZE,Tensor3D::MEMSIZE) {}
  
  // constructor (also serves as copy constructor)
  Tensor4_3D(const ShortSqrMatrix& M,unsigned int idx0 = 0)
    : ShortMatrix(M,Tensor3D::MEMSIZE,Tensor3D::MEMSIZE,idx0,idx0) {}
  
  // destructor
  virtual ~Tensor4_3D() {}
  
  // assignment operator
  Tensor4_3D& operator=(const Tensor4_3D& src) throw (std::range_error) {
    ShortSqrMatrix::operator=(src);
    return *this;
  }
  Tensor4_3D& operator=(const ShortMatrix& src) throw (std::range_error) {
    ShortSqrMatrix::operator=(src);
    return *this;
  }
  Tensor4_3D& operator=(const ShortMatrixExpression& src) throw (std::range_error) {
    ShortSqrMatrix::operator=(src);
    return *this;
  }
  Tensor4_3D& operator=(double v) {
    ShortSqrMatrix::operator=(v);
    return *this;
  }
  
  // specific operation (+= coef*A_il*A_kj)
  void addIJKL(double,const Tensor3D&);
  
  // specific operation (+= coef*A_il*B_kj)
  void addIJKL(double,const Tensor3D&,const Tensor3D&);
  
  // identity tensor
  static Tensor4_3D identity();
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif


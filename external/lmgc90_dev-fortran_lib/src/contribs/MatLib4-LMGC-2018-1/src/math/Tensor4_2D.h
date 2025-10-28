/*
 *  $Id: Tensor4_2D.h 133 2013-07-22 18:47:44Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_TENSOR4_2D_H
#define ZORGLIB_MATH_TENSOR4_2D_H

// config
#include <matlib_macros.h>

// local
#include <data/ShortSqrMatrix.h>
#include <math/Tensor2D.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class encapsulating 2D 4th-order tensor objects.
 * It inherits methods and internal structure from ShortSqrMatrix.
 */
class Tensor4_2D : virtual public ShortSqrMatrix {
  
 public:
  
  // default constructor
  Tensor4_2D() : ShortMatrix(Tensor2D::MEMSIZE,Tensor2D::MEMSIZE) {}
  
  // constructor (also serves as copy constructor)
  Tensor4_2D(const ShortSqrMatrix& M,unsigned int idx0 = 0)
    : ShortMatrix(M,Tensor2D::MEMSIZE,Tensor2D::MEMSIZE,idx0,idx0) {}
  
  // destructor
  virtual ~Tensor4_2D() {}
  
  // assignment operator
  Tensor4_2D& operator=(const Tensor4_2D& src) throw (std::range_error) {
    ShortSqrMatrix::operator=(src);
    return *this;
  }
  Tensor4_2D& operator=(const ShortMatrix& src) throw (std::range_error) {
    ShortSqrMatrix::operator=(src);
    return *this;
  }
  Tensor4_2D& operator=(const ShortMatrixExpression& src) throw (std::range_error) {
    ShortSqrMatrix::operator=(src);
    return *this;
  }
  Tensor4_2D& operator=(double v) {
    ShortSqrMatrix::operator=(v);
    return *this;
  }
  
  // specific operation (+= coef*A_il*A_kj)
  void addIJKL(double,const Tensor2D&);
  
  // specific operation (+= coef*A_il*B_kj)
  void addIJKL(double,const Tensor2D&,const Tensor2D&);
  
  // identity tensor
  static Tensor4_2D identity();
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif


/*
 *  $Id: SymTensor4_2D.h 137 2013-08-30 15:20:05Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_SYM_TENSOR4_2D_H
#define ZORGLIB_MATH_SYM_TENSOR4_2D_H

// config
#include <matlib_macros.h>

// local
#include <data/ShortSqrMatrix.h>
#include <math/SymTensor2D.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class encapsulating 2D symmetric 4th-order tensor objects.
 * It inherits methods and internal structure from ShortSqrMatrix.
 */
class SymTensor4_2D : virtual public ShortSqrMatrix {
  
 public:
  
  // default constructor
  SymTensor4_2D() : ShortMatrix(SymTensor2D::MEMSIZE,SymTensor2D::MEMSIZE) {}
  
  // constructor (also serves as copy constructor)
  SymTensor4_2D(const ShortSqrMatrix& M,unsigned int idx0 = 0)
    : ShortMatrix(M,SymTensor2D::MEMSIZE,SymTensor2D::MEMSIZE,idx0,idx0) {}
  
  // constructor
  SymTensor4_2D(const ShortMatrixExpression& M,unsigned int idx0 = 0)
    : ShortMatrix(M,SymTensor2D::MEMSIZE,SymTensor2D::MEMSIZE,idx0,idx0) {}

  // destructor
  virtual ~SymTensor4_2D() {}
  
  // assignment operator
  SymTensor4_2D& operator=(const SymTensor4_2D& src) throw (std::range_error) {
    ShortSqrMatrix::operator=(src);
    return *this;
  }
  SymTensor4_2D& operator=(const ShortMatrix& src) throw (std::range_error) {
    ShortSqrMatrix::operator=(src);
    return *this;
  }
  SymTensor4_2D& operator=(const ShortMatrixExpression& src) throw (std::range_error) {
    ShortSqrMatrix::operator=(src);
    return *this;
  }
  SymTensor4_2D& operator=(double v) {
    ShortSqrMatrix::operator=(v);
    return *this;
  }
  
  // access operators
  double operator()(unsigned int,unsigned int,unsigned int,unsigned int) const throw (std::out_of_range);
  double& operator()(unsigned int,unsigned int,unsigned int,unsigned int) throw (std::out_of_range);
  
  // specific operation (+= coef*(A_ik*A_jl+A_il*A_jk))
  void addIJKL(double,const SymTensor2D&);
  
  // specific operation (+= coef*(A_ik*B_jl+A_il*B_jk))
  void addIJKL(double,const SymTensor2D&,const SymTensor2D&);
  
  // push-pull operations
  SymTensor4_2D covariantPush(const Tensor2D&) const;
  SymTensor4_2D covariantPull(const Tensor2D&) const;
  SymTensor4_2D contravariantPush(const Tensor2D&) const;
  SymTensor4_2D contravariantPull(const Tensor2D&) const;
  
  // identity tensor
  static SymTensor4_2D identity();
  static SymTensor4_2D contravariantIdentity();
  static SymTensor4_2D covariantIdentity();
  
  // tensorial bases
  static SymTensor4_2D baseJ();
  static SymTensor4_2D baseK();
};

// full inner product
SymTensor2D innerProd2(const SymTensor4_2D&,const SymTensor2D&);
SymTensor4_2D innerProd2(const SymTensor4_2D&,const SymTensor4_2D&);

// symmetric part of product between symmetric 4th-order and 2nd-order tensors
SymTensor4_2D symProd(const SymTensor4_2D&,const SymTensor2D&);
SymTensor4_2D symProd(const SymTensor2D&,const SymTensor4_2D&);

// push-pull operations
inline 
SymTensor4_2D covariantPush(const SymTensor4_2D& A,const Tensor2D& B) {
  return A.covariantPush(B);
}
inline 
SymTensor4_2D covariantPull(const SymTensor4_2D& A,const Tensor2D& B) {
  return A.covariantPull(B);
}
inline 
SymTensor4_2D contravariantPush(const SymTensor4_2D& A,const Tensor2D& B) {
  return A.contravariantPush(B);
}
inline 
SymTensor4_2D contravariantPull(const SymTensor4_2D& A,const Tensor2D& B) {
  return A.contravariantPull(B);
}

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif


/*
 *  $Id: SymTensor4_1D.h 137 2013-08-30 15:20:05Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_SYM_TENSOR4_1D_H
#define ZORGLIB_MATH_SYM_TENSOR4_1D_H

// config
#include <matlib_macros.h>

// local
#include <data/ShortSqrMatrix.h>
#include <math/SymTensor1D.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class encapsulating 1D symmetric 4th-order tensor objects.
 * It inherits methods and internal structure from ShortSqrMatrix.
 */
class SymTensor4_1D : virtual public ShortSqrMatrix {
  
 public:
  
  // default constructor
  SymTensor4_1D() : ShortMatrix(SymTensor1D::MEMSIZE,SymTensor1D::MEMSIZE) {}
  
  // constructor (also serves as copy constructor)
  SymTensor4_1D(const ShortSqrMatrix& M,unsigned int idx0 = 0)
    : ShortMatrix(M,SymTensor1D::MEMSIZE,SymTensor1D::MEMSIZE,idx0,idx0) {}
  
  // constructor
  SymTensor4_1D(const ShortMatrixExpression& M,unsigned int idx0 = 0)
    : ShortMatrix(M,SymTensor1D::MEMSIZE,SymTensor1D::MEMSIZE,idx0,idx0) {}

  // destructor
  virtual ~SymTensor4_1D() {}
  
  // assignment operator
  SymTensor4_1D& operator=(const SymTensor4_1D& src) throw (std::range_error) {
    ShortSqrMatrix::operator=(src);
    return *this;
  }
  SymTensor4_1D& operator=(const ShortMatrix& src) throw (std::range_error) {
    ShortSqrMatrix::operator=(src);
    return *this;
  }
  SymTensor4_1D& operator=(const ShortMatrixExpression& src) throw (std::range_error) {
    ShortSqrMatrix::operator=(src);
    return *this;
  }
  SymTensor4_1D& operator=(double v) {
    ShortSqrMatrix::operator=(v);
    return *this;
  }
  
  // access operators
  double operator()(unsigned int,unsigned int,unsigned int,unsigned int) const throw (std::out_of_range);
  double& operator()(unsigned int,unsigned int,unsigned int,unsigned int) throw (std::out_of_range);
  
  // specific operation (+= coef*(A_ik*A_jl+A_il*A_jk))
  void addIJKL(double,const SymTensor1D&);
  
  // specific operation (+= coef*(A_ik*B_jl+A_il*B_jk))
  void addIJKL(double,const SymTensor1D&,const SymTensor1D&);
  
  // push-pull operations
  SymTensor4_1D covariantPush(const SymTensor1D&) const;
  SymTensor4_1D covariantPull(const SymTensor1D&) const;
  SymTensor4_1D contravariantPush(const SymTensor1D&) const;
  SymTensor4_1D contravariantPull(const SymTensor1D&) const;
  
  // identity tensor
  static SymTensor4_1D identity();
  static SymTensor4_1D contravariantIdentity();
  static SymTensor4_1D covariantIdentity();
  
  // tensorial bases
  static SymTensor4_1D baseJ();
  static SymTensor4_1D baseK();
};

// full inner product
SymTensor1D innerProd2(const SymTensor4_1D&,const SymTensor1D&);
SymTensor4_1D innerProd2(const SymTensor4_1D&,const SymTensor4_1D&);

// symmetric part of product between symmetric 4th-order and 2nd-order tensors
SymTensor4_1D symProd(const SymTensor4_1D&,const SymTensor1D&);
SymTensor4_1D symProd(const SymTensor1D&,const SymTensor4_1D&);

// push-pull operations
inline 
SymTensor4_1D covariantPush(const SymTensor4_1D& A,const SymTensor1D& B) {
  return A.covariantPush(B);
}
inline 
SymTensor4_1D covariantPull(const SymTensor4_1D& A,const SymTensor1D& B) {
  return A.covariantPull(B);
}
inline 
SymTensor4_1D contravariantPush(const SymTensor4_1D& A,const SymTensor1D& B) {
  return A.contravariantPush(B);
}
inline 
SymTensor4_1D contravariantPull(const SymTensor4_1D& A,const SymTensor1D& B) {
  return A.contravariantPull(B);
}

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif


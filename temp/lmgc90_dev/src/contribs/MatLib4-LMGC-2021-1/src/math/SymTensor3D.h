/*
 *  $Id: SymTensor3D.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_SYM_TENSOR_3D_H
#define ZORGLIB_MATH_SYM_TENSOR_3D_H

// config
#include <matlib_macros.h>

// local
#include <data/ShortArray.h>
#include <data/ShortSqrMatrix.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

// forward declaration
class Tensor3D;
class Vector3D;

/**
 * Class encapsulating 3D symmetric tensor objects.
 * It inherits methods and internal structure from ShortArray.
 */
class SymTensor3D : virtual public ShortArray {
  
 public:
  
  static const unsigned int MEMSIZE=6;
  
  // index map
  static const int MAP[3][3];
  
  // default constructor
  SymTensor3D() : ShortArray(MEMSIZE) {}
  
  // constructor (also serves as copy constructor)
  SymTensor3D(const ShortArray& a,unsigned int idx0 = 0) : ShortArray(a,MEMSIZE,idx0) {}
  
  // constructor
  SymTensor3D(const ShortArrayExpression& a,unsigned int idx0 = 0)
    : ShortArray(a,MEMSIZE,idx0) {}
  
  // destructor
  virtual ~SymTensor3D() {}
  
  // assignment operator
  SymTensor3D& operator=(const SymTensor3D& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  SymTensor3D& operator=(const ShortArray& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  SymTensor3D& operator=(const ShortArrayExpression& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  SymTensor3D& operator=(double v) {
    ShortArray::operator=(v);
    return *this;
  }

  // specific arithmetic operators
  Tensor3D operator*(const Tensor3D&) const;
  Vector3D operator*(const Vector3D&) const;

  // compute determinant
  double determinant() const;
  
  // compute eigenvalues
  void eigenValues(double[]) const;

  // compute eigenvalues and eigenbases
  void eigenSplit(double[],SymTensor3D[]) const;

  // compute trace
  double trace() const;
  
  // transform to covariant
  SymTensor3D covariant() const;
  
  // transform to contravariant
  SymTensor3D contravariant() const;

  // push-pull operations
  SymTensor3D covariantPush(const Tensor3D&) const;
  SymTensor3D covariantPull(const Tensor3D&) const;
  SymTensor3D contravariantPush(const Tensor3D&) const;
  SymTensor3D contravariantPull(const Tensor3D&) const;
  
  // compute inverse
  SymTensor3D inverse(double&) const;
  
  // compute exponential
  SymTensor3D exp(SymTensor3D[] = 0,SymTensor3D[][MEMSIZE] = 0,
                  bool = false,bool = false) const;
  
  // compute logarithm
  SymTensor3D log(SymTensor3D[] = 0,SymTensor3D[][MEMSIZE] = 0,
                  bool = false,bool = false) const;

  // identity tensor
  static SymTensor3D identity();
  
  // build tensor by vector outer product
  static SymTensor3D outerProd(const Vector3D&);
  static SymTensor3D outerProd(const Vector3D&,const Vector3D&);
  
  // export as square matrix
  ShortSqrMatrix toMatrix() const;
};

// compute determinant
inline
double determinant(const SymTensor3D& A) {return A.determinant();}

// compute trace
inline
double trace(const SymTensor3D& A) {return A.trace();}

// transform to covariant
inline
SymTensor3D covariant(const SymTensor3D& A) {return A.covariant();}

// transform to contravariant
inline
SymTensor3D contravariant(const SymTensor3D& A) {return A.contravariant();}

// push-pull operations
inline 
SymTensor3D covariantPush(const SymTensor3D& A,const Tensor3D& B) {
  return A.covariantPush(B);
}
inline 
SymTensor3D covariantPull(const SymTensor3D& A,const Tensor3D& B) {
  return A.covariantPull(B);
}
inline 
SymTensor3D contravariantPush(const SymTensor3D& A,const Tensor3D& B) {
  return A.contravariantPush(B);
}
inline 
SymTensor3D contravariantPull(const SymTensor3D& A,const Tensor3D& B) {
  return A.contravariantPull(B);
}

// compute inverse
inline
SymTensor3D invert(const SymTensor3D& A) {double d; return A.inverse(d);}

//compute exponential
inline
SymTensor3D exp(const SymTensor3D& A,SymTensor3D dExpA[] = 0,
                SymTensor3D d2ExpA[][SymTensor3D::MEMSIZE] = 0,
                bool first = false,bool second = false) {
  return A.exp(dExpA,d2ExpA,first,second);
}

//compute logarithm
inline
SymTensor3D log(const SymTensor3D& A,SymTensor3D dLogA[] = 0,
                SymTensor3D d2LogA[][SymTensor3D::MEMSIZE] = 0,
                bool first = false,bool second = false) {
  return A.log(dLogA,d2LogA,first,second);
}

// full inner product
double innerProd2(const SymTensor3D&,const SymTensor3D&);

// symmetric part of product between two symmetric tensors
SymTensor3D symProd(const SymTensor3D&,const SymTensor3D&);

// tensor product of two identical vectors
inline
SymTensor3D symTensorProd(const Vector3D& v) {
  return SymTensor3D::outerProd(v);
}

// symmetric part of tensor product between two vectors
inline
SymTensor3D symTensorProd(const Vector3D& a,const Vector3D& b) {
  return SymTensor3D::outerProd(a,b);
}

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif


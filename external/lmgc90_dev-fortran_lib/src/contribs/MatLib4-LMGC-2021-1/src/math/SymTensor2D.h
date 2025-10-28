/*
 *  $Id: SymTensor2D.h 130 2013-04-11 01:18:02Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_SYM_TENSOR_2D_H
#define ZORGLIB_MATH_SYM_TENSOR_2D_H

// config
#include <matlib_macros.h>

// local
#include <data/ShortArray.h>
#include <data/ShortSqrMatrix.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

// forward declaration
class Tensor2D;
class Vector2D;
class Vector3D;

/**
 * Class encapsulating 2D symmetric tensor objects.
 * It inherits methods and internal structure from ShortArray.
 */
class StdSymTensor2D : virtual public ShortArray {
  
 public:
  
  static const unsigned int MEMSIZE=3;
  
  // index map
  static const int MAP[2][2];
  
  // default constructor
  StdSymTensor2D() : ShortArray(MEMSIZE) {}
  
  // constructor (also serves as copy constructor)
  StdSymTensor2D(const ShortArray& a,unsigned int idx0 = 0) : ShortArray(a,MEMSIZE,idx0) {}
  
  // constructor
  StdSymTensor2D(const ShortArrayExpression& a,unsigned int idx0 = 0)
	: ShortArray(a,MEMSIZE,idx0) {}
  
  // destructor
  virtual ~StdSymTensor2D() {}
  
  // assignment operator
  StdSymTensor2D& operator=(const StdSymTensor2D& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  StdSymTensor2D& operator=(const ShortArray& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  StdSymTensor2D& operator=(const ShortArrayExpression& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  StdSymTensor2D& operator=(double v) {
    ShortArray::operator=(v);
    return *this;
  }
  
  // compute determinant
  double determinant() const;
  
  // compute eigenvalues
  void eigenValues(double[]) const;
  
  // compute eigenvalues and eigenbases
  void eigenSplit(double[],StdSymTensor2D[]) const;
  
  // compute trace
  double trace() const;
  
  // transform to covariant
  StdSymTensor2D covariant() const;
  
  // transform to contravariant
  StdSymTensor2D contravariant() const;
  
  // compute inverse
  StdSymTensor2D inverse(double&) const;
  
  // compute exponential
  StdSymTensor2D exp(StdSymTensor2D[] = 0,StdSymTensor2D[][MEMSIZE] = 0,
                     bool = false,bool = false) const;
  
  // compute logarithm
  StdSymTensor2D log(StdSymTensor2D[] = 0,StdSymTensor2D[][MEMSIZE] = 0,
                     bool = false,bool = false) const;
  
  // identity tensor
  static StdSymTensor2D identity();
  
  // build tensor by vector outer product
  static StdSymTensor2D outerProd(const Vector2D&);
  static StdSymTensor2D outerProd(const Vector2D&,const Vector2D&);
  
  // export as square matrix
  ShortSqrMatrix toMatrix() const;
};

// compute determinant
inline
double determinant(const StdSymTensor2D& A) {return A.determinant();}

// compute trace
inline
double trace(const StdSymTensor2D& A) {return A.trace();}

// transform to covariant
inline
StdSymTensor2D covariant(const StdSymTensor2D& A) {return A.covariant();}

// transform to contravariant
inline
StdSymTensor2D contravariant(const StdSymTensor2D& A) {return A.contravariant();}

//compute exponential
inline
StdSymTensor2D exp(const StdSymTensor2D& A,StdSymTensor2D dExpA[] = 0,
                   StdSymTensor2D d2ExpA[][StdSymTensor2D::MEMSIZE] = 0,
                   bool first = false,bool second = false) {
  return A.exp(dExpA,d2ExpA,first,second);
}

//compute logarithm
inline
StdSymTensor2D log(const StdSymTensor2D& A,StdSymTensor2D dLogA[] = 0,
                   StdSymTensor2D d2LogA[][StdSymTensor2D::MEMSIZE] = 0,
                   bool first = false,bool second = false) {
  return A.log(dLogA,d2LogA,first,second);
}

// full inner product
double innerProd2(const StdSymTensor2D&,const StdSymTensor2D&);

// tensor product of two identical vectors
inline
StdSymTensor2D symTensorProd(const Vector2D& v) {
  return StdSymTensor2D::outerProd(v);
}

// symmetric part of tensor product between two vectors
inline
StdSymTensor2D symTensorProd(const Vector2D& a,const Vector2D& b) {
  return StdSymTensor2D::outerProd(a,b);
}


/**
 * Class encapsulating semi-2D symmetric tensor objects, as used in mechanics.
 * It inherits methods and internal structure from ShortArray.
 */
class SymTensor2D : virtual public ShortArray {
  
 public:
  
  static const unsigned int MEMSIZE=4;
  
  // index map
  static const int MAP[3][3];
  
  // default constructor
  SymTensor2D() : ShortArray(MEMSIZE) {}
  
  // constructor (also serves as copy constructor)
  SymTensor2D(const ShortArray& a,unsigned int idx0 = 0) : ShortArray(a,MEMSIZE,idx0) {}
  
  // constructor
  SymTensor2D(const ShortArrayExpression& a,unsigned int idx0 = 0) 
	: ShortArray(a,MEMSIZE,idx0) {}
  
  // destructor
  virtual ~SymTensor2D() {}
  
  // assignment operator
  SymTensor2D& operator=(const SymTensor2D& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  SymTensor2D& operator=(const ShortArray& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  SymTensor2D& operator=(const ShortArrayExpression& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  SymTensor2D& operator=(double v) {
    ShortArray::operator=(v);
    return *this;
  }
  
  // specific arithmetic operators
  Tensor2D operator*(const Tensor2D&) const;
  Vector2D operator*(const Vector2D&) const;
  Vector3D operator*(const Vector3D&) const;

  // compute determinant
  double determinant() const;
  
  // compute eigenvalues
  void eigenValues(double[]) const;
  
  // compute eigenvalues and eigenbases
  void eigenSplit(double[],SymTensor2D[]) const;
  
  // compute trace
  double trace() const;
  
  // transform to covariant
  SymTensor2D covariant() const;
  
  // transform to contravariant
  SymTensor2D contravariant() const;
  
  // push-pull operations
  SymTensor2D covariantPush(const Tensor2D&) const;
  SymTensor2D covariantPull(const Tensor2D&) const;
  SymTensor2D contravariantPush(const Tensor2D&) const;
  SymTensor2D contravariantPull(const Tensor2D&) const;
  
  // compute inverse
  SymTensor2D inverse(double&) const;
  
  // compute exponential
  SymTensor2D exp(SymTensor2D[] = 0,SymTensor2D[][MEMSIZE] = 0,
                  bool = false,bool = false) const;
  
  // compute logarithm
  SymTensor2D log(SymTensor2D[] = 0,SymTensor2D[][MEMSIZE] = 0,
                  bool = false,bool = false) const;
  
  // identity tensor
  static SymTensor2D identity();
  
  // build tensor by vector outer product
  static SymTensor2D outerProd(const Vector3D&);
  static SymTensor2D outerProd(const Vector2D&);
  static SymTensor2D outerProd(const Vector3D&,const Vector3D&);
  static SymTensor2D outerProd(const Vector2D&,const Vector2D&);
  
  // export as square matrix
  ShortSqrMatrix toMatrix() const;
};

// compute determinant
inline
double determinant(const SymTensor2D& A) {return A.determinant();}

// compute trace
inline
double trace(const SymTensor2D& A) {return A.trace();}

// transform to covariant
inline
SymTensor2D covariant(const SymTensor2D& A) {return A.covariant();}

// transform to contravariant
inline
SymTensor2D contravariant(const SymTensor2D& A) {return A.contravariant();}

// push-pull operations
inline 
SymTensor2D covariantPush(const SymTensor2D& A,const Tensor2D& B) {
  return A.covariantPush(B);
}
inline 
SymTensor2D covariantPull(const SymTensor2D& A,const Tensor2D& B) {
  return A.covariantPull(B);
}
inline 
SymTensor2D contravariantPush(const SymTensor2D& A,const Tensor2D& B) {
  return A.contravariantPush(B);
}
inline 
SymTensor2D contravariantPull(const SymTensor2D& A,const Tensor2D& B) {
  return A.contravariantPull(B);
}

//compute exponential
inline
SymTensor2D exp(const SymTensor2D& A,SymTensor2D dExpA[] = 0,
                SymTensor2D d2ExpA[][SymTensor2D::MEMSIZE] = 0,
                bool first = false,bool second = false) {
  return A.exp(dExpA,d2ExpA,first,second);
}

//compute logarithm
inline
SymTensor2D log(const SymTensor2D& A,SymTensor2D dLogA[] = 0,
                SymTensor2D d2LogA[][SymTensor2D::MEMSIZE] = 0,
                bool first = false,bool second = false) {
  return A.log(dLogA,d2LogA,first,second);
}

// full inner product
double innerProd2(const SymTensor2D&,const SymTensor2D&);

// symmetric part of product between two symmetric tensors
SymTensor2D symProd(const SymTensor2D&,const SymTensor2D&);

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif


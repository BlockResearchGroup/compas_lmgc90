/*
 *  $Id: SymTensor1D.h 130 2013-04-11 01:18:02Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_SYM_TENSOR_1D_H
#define ZORGLIB_MATH_SYM_TENSOR_1D_H

// config
#include <matlib_macros.h>

// local
#include <data/ShortArray.h>
#include <data/ShortSqrMatrix.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

// forward declarations
class Vector1D;
class Vector3D;

/**
 * Class encapsulating standard 1D tensor objects (always symmetric).
 * It inherits methods and internal structure from ShortArray.
 */
class StdSymTensor1D : virtual public ShortArray {
  
 public:
  
  static const unsigned int MEMSIZE=1;
  
  // index map
  static const int MAP[1][1];
  
  // default constructor
  StdSymTensor1D() : ShortArray(MEMSIZE) {}
  
  // constructor (also serves as copy constructor)
  StdSymTensor1D(const ShortArray& a,unsigned int idx0 = 0) : ShortArray(a,MEMSIZE,idx0) {}
  
  // constructor
  StdSymTensor1D(const ShortArrayExpression& a,unsigned int idx0 = 0)
	: ShortArray(a,MEMSIZE,idx0) {}
  
  // destructor
  virtual ~StdSymTensor1D() {}
  
  // assignment operator
  StdSymTensor1D& operator=(const StdSymTensor1D& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  StdSymTensor1D& operator=(const ShortArray& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  StdSymTensor1D& operator=(const ShortArrayExpression& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  StdSymTensor1D& operator=(double v) {
    ShortArray::operator=(v);
    return *this;
  }
  
  // specific arithmetic operators
  //StdSymTensor1D operator*(const StdSymTensor1D&) const;
  
  // compute determinant
  double determinant() const;
  
  // compute eigenvalues
  void eigenValues(double[]) const;
  
  // compute eigenvalues and eigenbases
  void eigenSplit(double[],StdSymTensor1D[]) const;
  
  // compute trace
  double trace() const;
  
  // transform to covariant
  StdSymTensor1D covariant() const {return StdSymTensor1D(*this);}
  
  // transform to contravariant
  StdSymTensor1D contravariant() const {return StdSymTensor1D(*this);}
  
  // push-pull operations
  StdSymTensor1D covariantPush(const StdSymTensor1D&) const;
  StdSymTensor1D covariantPull(const StdSymTensor1D&) const;
  StdSymTensor1D contravariantPush(const StdSymTensor1D&) const;
  StdSymTensor1D contravariantPull(const StdSymTensor1D&) const;
  
  // compute inverse
  StdSymTensor1D inverse(double&) const;
  
  // compute exponential
  StdSymTensor1D exp(StdSymTensor1D[] = 0,StdSymTensor1D[][MEMSIZE] = 0,
                     bool = false,bool = false) const;
  
  // compute logarithm
  StdSymTensor1D log(StdSymTensor1D[] = 0,StdSymTensor1D[][MEMSIZE] = 0,
                     bool = false,bool = false) const;
  
  // identity tensor
  static StdSymTensor1D identity();
  
  // build tensor by vector outer product
  static StdSymTensor1D outerProd(const Vector1D&);
  static StdSymTensor1D outerProd(const Vector1D&,const Vector1D&);
  
  // export as square matrix
  ShortSqrMatrix toMatrix() const;
};

// compute determinant
inline
double determinant(const StdSymTensor1D& A) {return A.determinant();}

// compute trace
inline
double trace(const StdSymTensor1D& A) {return A.trace();}

// transform to covariant
inline
StdSymTensor1D covariant(const StdSymTensor1D& A) {return A.covariant();}

// transform to contravariant
inline
StdSymTensor1D contravariant(const StdSymTensor1D& A) {return A.contravariant();}

//compute exponential
inline
StdSymTensor1D exp(const StdSymTensor1D& A,StdSymTensor1D dExpA[] = 0,
                   StdSymTensor1D d2ExpA[][StdSymTensor1D::MEMSIZE] = 0,
                   bool first = false,bool second = false) {
  return A.exp(dExpA,d2ExpA,first,second);
}

//compute logarithm
inline
StdSymTensor1D log(const StdSymTensor1D& A,StdSymTensor1D dLogA[] = 0,
                   StdSymTensor1D d2LogA[][StdSymTensor1D::MEMSIZE] = 0,
                   bool first = false,bool second = false) {
  return A.log(dLogA,d2LogA,first,second);
}

// full inner product
double innerProd2(const StdSymTensor1D&,const StdSymTensor1D&);

// tensor product of two identical vectors
inline
StdSymTensor1D symTensorProd(const Vector1D& v) {
  return StdSymTensor1D::outerProd(v);
}

// symmetric part of tensor product between two vectors
inline
StdSymTensor1D symTensorProd(const Vector1D& a,const Vector1D& b) {
  return StdSymTensor1D::outerProd(a,b);
}


/**
 * Class encapsulating semi-1D tensor objects, as used for mechanics.
 * It inherits methods and internal structure from ShortArray.
 */
class SymTensor1D : virtual public ShortArray {
  
 public:
  
  static const unsigned int MEMSIZE=3;
  
  // index map
  static const int MAP[3][3];
  
  // default constructor
  SymTensor1D() : ShortArray(MEMSIZE) {}
  
  // constructor (also serves as copy constructor)
  SymTensor1D(const ShortArray& a,unsigned int idx0 = 0) : ShortArray(a,MEMSIZE,idx0) {}
  
  // constructor
  SymTensor1D(const ShortArrayExpression& a,unsigned int idx0 = 0)
	: ShortArray(a,MEMSIZE,idx0) {}
  
  // destructor
  virtual ~SymTensor1D() {}
  
  // assignment operator
  SymTensor1D& operator=(const SymTensor1D& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  SymTensor1D& operator=(const ShortArray& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  SymTensor1D& operator=(const ShortArrayExpression& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  SymTensor1D& operator=(double v) {
    ShortArray::operator=(v);
    return *this;
  }
  
  // specific arithmetic operators
  SymTensor1D operator*(const SymTensor1D&) const;
  Vector1D operator*(const Vector1D&) const;
  Vector3D operator*(const Vector3D&) const;
 
  // compute determinant
  double determinant() const;
  
  // compute eigenvalues
  void eigenValues(double[]) const;
  
  // compute eigenvalues and eigenbases
  void eigenSplit(double[],SymTensor1D[]) const;
  
  // compute trace
  double trace() const;
  
  // transform to covariant
  SymTensor1D covariant() const {return SymTensor1D(*this);}
  
  // transform to contravariant
  SymTensor1D contravariant() const {return SymTensor1D(*this);}
  
  // push-pull operations
  SymTensor1D covariantPush(const SymTensor1D&) const;
  SymTensor1D covariantPull(const SymTensor1D&) const;
  SymTensor1D contravariantPush(const SymTensor1D&) const;
  SymTensor1D contravariantPull(const SymTensor1D&) const;
  
  // compute inverse
  SymTensor1D inverse(double&) const;
  
  // compute exponential
  SymTensor1D exp(SymTensor1D[] = 0,SymTensor1D[][MEMSIZE] = 0,
                  bool = false,bool = false) const;
  
  // compute logarithm
  SymTensor1D log(SymTensor1D[] = 0,SymTensor1D[][MEMSIZE] = 0,
                  bool = false,bool = false) const;
  
  // identity tensor
  static SymTensor1D identity();
  
  // build tensor by vector outer product
  static SymTensor1D outerProd(const Vector3D&);
  static SymTensor1D outerProd(const Vector1D&);
  static SymTensor1D outerProd(const Vector3D&,const Vector3D&);
  static SymTensor1D outerProd(const Vector1D&,const Vector1D&);
  
  // export as square matrix
  ShortSqrMatrix toMatrix() const;
};

// compute determinant
inline
double determinant(const SymTensor1D& A) {return A.determinant();}

// compute trace
inline
double trace(const SymTensor1D& A) {return A.trace();}

// transform to covariant
inline
SymTensor1D covariant(const SymTensor1D& A) {return A.covariant();}

// transform to contravariant
inline
SymTensor1D contravariant(const SymTensor1D& A) {return A.contravariant();}

// push-pull operations
inline 
SymTensor1D covariantPush(const SymTensor1D& A,const SymTensor1D& B) {
  return A.covariantPush(B);
}
inline 
SymTensor1D covariantPull(const SymTensor1D& A,const SymTensor1D& B) {
  return A.covariantPull(B);
}
inline 
SymTensor1D contravariantPush(const SymTensor1D& A,const SymTensor1D& B) {
  return A.contravariantPush(B);
}
inline 
SymTensor1D contravariantPull(const SymTensor1D& A,const SymTensor1D& B) {
  return A.contravariantPull(B);
}

//compute exponential
inline
SymTensor1D exp(const SymTensor1D& A,SymTensor1D dExpA[] = 0,
                SymTensor1D d2ExpA[][SymTensor1D::MEMSIZE] = 0,
                bool first = false,bool second = false) {
  return A.exp(dExpA,d2ExpA,first,second);
}

//compute logarithm
inline
SymTensor1D log(const SymTensor1D& A,SymTensor1D dLogA[] = 0,
                SymTensor1D d2LogA[][SymTensor1D::MEMSIZE] = 0,
                bool first = false,bool second = false) {
  return A.log(dLogA,d2LogA,first,second);
}

// full inner product
double innerProd2(const SymTensor1D&,const SymTensor1D&);

// symmetric part of product between two symmetric tensors
SymTensor1D symProd(const SymTensor1D&,const SymTensor1D&);


#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif


/*
 *  $Id: Tensor3D.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_TENSOR_3D_H
#define ZORGLIB_MATH_TENSOR_3D_H

// config
#include <matlib_macros.h>

// local
#include <data/ShortArray.h>
#include <data/ShortSqrMatrix.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

// forward declaration
class SymTensor3D;
class Vector3D;

/**
 * Class encapsulating 3D tensor objects.
 * It inherits methods and internal structure from ShortArray.
 */
class Tensor3D : virtual public ShortArray {
  
 public:
  
  static const unsigned int MEMSIZE=9;
  
  // index map
  static const int MAP[3][3];
  
  // default constructor
  Tensor3D() : ShortArray(MEMSIZE) {}
  
  // constructor (also serves as copy constructor)
  Tensor3D(const ShortArray& a,unsigned int idx0 = 0) : ShortArray(a,MEMSIZE,idx0) {}
  
  // constructor
  Tensor3D(const ShortArrayExpression& a,unsigned int idx0 = 0)
	: ShortArray(a,MEMSIZE,idx0) {}
  
  // destructor
  virtual ~Tensor3D() {}
  
  // assignment operator
  Tensor3D& operator=(const Tensor3D& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  Tensor3D& operator=(const ShortArray& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  Tensor3D& operator=(const ShortArrayExpression& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  Tensor3D& operator=(double v) {
    ShortArray::operator=(v);
    return *this;
  }
  
  // specific arithmetic operators
  Tensor3D operator*(const Tensor3D&) const;
  Tensor3D operator*(const SymTensor3D&) const;
  Vector3D operator*(const Vector3D&) const;
  
  // symmetrize
  SymTensor3D covariantSym() const;
  SymTensor3D contravariantSym() const;
  
  // compute determinant
  double determinant() const;
  
  // compute trace
  double trace() const;

  // compute inverse
  Tensor3D inverse(double&) const;
  
  // compute transposed
  Tensor3D transposed() const;

  // compute exponential
  Tensor3D exp(Tensor3D[] = 0,Tensor3D[][MEMSIZE] = 0,
               bool = false,bool = false) const;
  
  // compute logarithm
  Tensor3D log(Tensor3D[] = 0,Tensor3D[][MEMSIZE] = 0,
               bool = false,bool = false) const;
  
  // identity tensor
  static Tensor3D identity();
  
  // export as square matrix
  ShortSqrMatrix toMatrix() const;
};

// compute determinant
inline
double determinant(const Tensor3D& A) {return A.determinant();}

// compute trace
inline
double trace(const Tensor3D& A) {return A.trace();}

// compute inverse
inline
Tensor3D invert(const Tensor3D& A) {double d; return A.inverse(d);}

// compute transposed
inline
Tensor3D transpose(const Tensor3D& A) {return A.transposed();}

//compute exponential
inline
Tensor3D exp(const Tensor3D& A,Tensor3D dExpA[] = 0,
             Tensor3D d2ExpA[][Tensor3D::MEMSIZE] = 0,
             bool first = false,bool second = false) {
  return A.exp(dExpA,d2ExpA,first,second);
}

//compute logarithm
inline
Tensor3D log(const Tensor3D& A,Tensor3D dLogA[] = 0,
             Tensor3D d2LogA[][Tensor3D::MEMSIZE] = 0,
             bool first = false,bool second = false) {
  return A.log(dLogA,d2LogA,first,second);
}

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif


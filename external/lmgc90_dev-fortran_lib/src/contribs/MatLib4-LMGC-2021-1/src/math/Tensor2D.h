/*
 *  $Id: Tensor2D.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_TENSOR_2D_H
#define ZORGLIB_MATH_TENSOR_2D_H

// config
#include <matlib_macros.h>

// local
#include <data/ShortArray.h>
#include <data/ShortSqrMatrix.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

// forward declaration
class SymTensor2D;
class Vector3D;

/**
 * Class encapsulating 2D tensor objects.
 * It inherits methods and internal structure from ShortArray.
 */
class Tensor2D : virtual public ShortArray {
  
 public:
  
  static const unsigned int MEMSIZE=5;
  
  // index map
  static const int MAP[3][3];
  
  // default constructor
  Tensor2D() : ShortArray(MEMSIZE) {}
  
  // constructor (also serves as copy constructor)
  Tensor2D(const ShortArray& a,unsigned int idx0 = 0) : ShortArray(a,MEMSIZE,idx0) {}
  
  // constructor
  Tensor2D(const ShortArrayExpression& a,unsigned int idx0 = 0)
	: ShortArray(a,MEMSIZE,idx0) {}
  
  // destructor
  virtual ~Tensor2D() {}
  
  // assignment operator
  Tensor2D& operator=(const Tensor2D& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  Tensor2D& operator=(const ShortArray& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  Tensor2D& operator=(const ShortArrayExpression& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  Tensor2D& operator=(double v) {
    ShortArray::operator=(v);
    return *this;
  }
  
  // specific arithmetic operators
  Tensor2D operator*(const Tensor2D&) const;
  Tensor2D operator*(const SymTensor2D&) const;
  Vector3D operator*(const Vector3D&) const;
  
  // symmetrize
  SymTensor2D covariantSym() const;
  SymTensor2D contravariantSym() const;
  
  // compute determinant
  double determinant() const;
  
  // compute trace
  double trace() const;

  // compute inverse
  Tensor2D inverse(double&) const;
  
  // compute transposed
  Tensor2D transposed() const;
  
  // compute exponential
  Tensor2D exp(Tensor2D[] = 0,Tensor2D[][MEMSIZE] = 0,
               bool = false,bool = false) const;
  
  // compute logarithm
  Tensor2D log(Tensor2D[] = 0,Tensor2D[][MEMSIZE] = 0,
               bool = false,bool = false) const;
  
  // identity tensor
  static Tensor2D identity();
  
  // export as square matrix
  ShortSqrMatrix toMatrix() const;
};

// compute determinant
inline
double determinant(const Tensor2D& A) {return A.determinant();}

// compute trace
inline
double trace(const Tensor2D& A) {return A.trace();}

// compute inverse
inline
Tensor2D invert(const Tensor2D& A) {double d; return A.inverse(d);}

// compute transposed
inline
Tensor2D transpose(const Tensor2D& A) {return A.transposed();}

//compute exponential
inline
Tensor2D exp(const Tensor2D& A,Tensor2D dExpA[] = 0,
             Tensor2D d2ExpA[][Tensor2D::MEMSIZE] = 0,
             bool first = false,bool second = false) {
  return A.exp(dExpA,d2ExpA,first,second);
}

//compute logarithm
inline
Tensor2D log(const Tensor2D& A,Tensor2D dLogA[] = 0,
             Tensor2D d2LogA[][Tensor2D::MEMSIZE] = 0,
             bool first = false,bool second = false) {
  return A.log(dLogA,d2LogA,first,second);
}

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif


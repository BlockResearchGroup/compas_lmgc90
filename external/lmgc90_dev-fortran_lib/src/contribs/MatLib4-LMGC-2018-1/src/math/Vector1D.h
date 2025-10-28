/*
 *  $Id: Vector1D.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_VECTOR_1D_H
#define ZORGLIB_MATH_VECTOR_1D_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include <data/ShortArray.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class encapsulating 1D vector objects.
 * It inherits methods and internal structure from ShortArray.
 */
class Vector1D : virtual public ShortArray {
  
 public:
  
  static const unsigned int MEMSIZE=1;
  
  // default constructor
  Vector1D() : ShortArray(MEMSIZE) {}
  
  // constructor (also serves as copy constructor)
  Vector1D(const ShortArray& a,unsigned int idx0 = 0) : ShortArray(a,MEMSIZE,idx0) {}
  Vector1D(const ShortArrayExpression& a,unsigned int idx0 = 0) : ShortArray(a,MEMSIZE,idx0) {}
  
  // destructor
  virtual ~Vector1D() {}
  
  // assignment operator
  Vector1D& operator=(const Vector1D& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  Vector1D& operator=(const ShortArrayExpression& src) throw (std::range_error) {
    ShortArray::operator=(src);
    return *this;
  }
  Vector1D& operator=(double v) {
    ShortArray::operator=(v);
    return *this;
  }
  
  // inner product operator
  double operator*(const Vector1D& other) const {
    return innerProd(*this,other);
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

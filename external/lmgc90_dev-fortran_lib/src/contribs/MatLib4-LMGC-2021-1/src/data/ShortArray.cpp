/*
 *  $Id: ShortArray.cpp 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "ShortArray.h"

// std C library
#include <cmath>
#include <cstring>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for ShortArrayExpression
 */

// print to string object
std::string ShortArrayExpression::toString() const {
  O_STRING_STREAM os;
  os << "[";
  for (unsigned int i=0; i < size(); i++)
    os << " " << (*this)[i];
  os << "]";
#ifdef HAVE_SSTREAM
  return os.str();
#else
  return std::string(os.str(),os.pcount());
#endif
}


/*
 * Methods for ShortArray
 */

// constructor
ShortArray::ShortArray(unsigned int s) {
  sz = s;
  if (s > 0)
    data = new double[s];
  else
    data = 0;
  v = data;
}

// subarray constructor
ShortArray::ShortArray(const ShortArray& src,unsigned int s,unsigned int idx0) {
  sz = s;
  if (s > 0)
    v = src.v+idx0;
  else
    v = 0;
  data = 0;
}
ShortArray::ShortArray(const ShortArrayExpression& src,unsigned int s,unsigned int idx0) {
  sz = s;
  if (s > 0) {
    data = new double[s];
    double* p = data;
    unsigned int j = idx0;
    for (unsigned int i=0; i < sz; i++, j++, p++) (*p) = src[j];
  }
  else
    data = 0;
  v = data;
}
  
// wrapper constructor (for the experienced user)
ShortArray::ShortArray(double* a,unsigned int s) {
  sz = s;
  v = a;
  data = 0;
}

// copy constructor
ShortArray::ShortArray(const ShortArray& src) {
  sz = src.sz;
  if (sz > 0) {
    data = new double[sz];
    std::memcpy(data,src.v,src.sz*sizeof(double));
  }
  else {
    data = 0;
  }
  v = data;
}
ShortArray::ShortArray(const ShortArrayExpression& src) {
  sz = src.size();
  if (sz > 0) {
    data = new double[sz];
    double* p = data;
    for (unsigned int i=0; i < sz; i++, p++) (*p) = src[i];
  }
  else {
    data = 0;
  }
  v = data;
}

// assignment operator
ShortArray& ShortArray::operator=(const ShortArray& src) throw (std::range_error) {
  if (size() != src.size()) throw std::range_error("ShortArray");
  if (size()) std::memcpy(v,src.v,src.size()*sizeof(double));
  return *this;
}
ShortArray& ShortArray::operator=(const ShortArrayExpression& src)
throw (std::range_error) {
  if (size() != src.size()) throw std::range_error("ShortArray");
  double* p = v;
  for (unsigned int i=0; i < src.size(); i++, p++) (*p) = src[i];
  return *this;
}
ShortArray& ShortArray::operator=(double val) {
  double* p = v;
  for (unsigned int i=0; i < size(); i++, p++) (*p) = val;
  return *this;
}

// unary operators
ShortArray& ShortArray::operator+=(const ShortArray& a) throw (std::range_error) {
  if (size() != a.size()) throw std::range_error("ShortArray");
  double* pv = v;
  double* pa = a.v;
  for (unsigned int i=0; i < a.size(); i++, pv++, pa++) (*pv) += (*pa);
  return *this;
}
ShortArray& ShortArray::operator+=(const ShortArrayExpression& a)
throw (std::range_error) {
  if (size() != a.size()) throw std::range_error("ShortArray");
  double* p = v;
  for (unsigned int i=0; i < a.size(); i++, p++) (*p) += a[i];
  return *this;
}
ShortArray& ShortArray::operator-=(const ShortArray& a) throw (std::range_error) {
  if (size() != a.size()) throw std::range_error("ShortArray");
  double* pv = v;
  double* pa = a.v;
  for (unsigned int i=0; i < a.size(); i++, pv++, pa++) (*pv) -= (*pa);
  return *this;
}
ShortArray& ShortArray::operator-=(const ShortArrayExpression& a)
throw (std::range_error) {
  if (size() != a.size()) throw std::range_error("ShortArray");
  double* p = v;
  for (unsigned int i=0; i < a.size(); i++, p++) (*p) -= a[i];
  return *this;
}
ShortArray& ShortArray::operator*=(double val) {
  double* p = v;
  for (unsigned int i=0; i < size(); i++, p++) (*p) *= val;
  return *this;
}
ShortArray& ShortArray::operator/=(double val) {
  double valInv = 1.0/val;
  double* p = v;
  for (unsigned int i=0; i < size(); i++, p++) (*p) *= valInv;
  return *this;
}

// resize
void ShortArray::resize(unsigned int s) {
  if (size() == s) return;
  if (data) delete [] data;
  sz = s;
  if (s > 0) 
    data = new double[s];
  else
    data = 0;
  v = data;
}

// wrap C array (for the experienced user)
void ShortArray::wrap(double* a,unsigned int s) {
  if (data) delete [] data;
  sz = s;
  v = a;
  data = 0;
}

// access operators
double ShortArray::operator()(unsigned int i) const throw (std::out_of_range) {
  if (i < size())
    return v[i];
  else
    throw std::out_of_range("ShortArray");
}
double& ShortArray::operator()(unsigned int i) throw (std::out_of_range) {
  if (i < size())
    return v[i];
  else
    throw std::out_of_range("ShortArray");
}

// print to string object
std::string ShortArray::toString() const {
  O_STRING_STREAM os;
  os << "[";
  for (unsigned int i=0; i < this->size(); i++)
    os << " " << v[i];
  os << "]";
#ifdef HAVE_SSTREAM
  return os.str();
#else
  return std::string(os.str(),os.pcount());
#endif
}

// define inner product
double MATLIB_NAMESPACE innerProd(const ShortArray& a,const ShortArray& b)
 throw (std::range_error) {
  if (a.size() != b.size()) throw std::range_error("innerProd");
  double prod = 0.e0;
  for (unsigned int i=0; i < a.size(); i++) prod += a[i]*b[i];
  return prod;
}

// compute norm of an array
double MATLIB_NAMESPACE normL1(const ShortArray& a) {
  double norm = 0.e0;
  for (unsigned int i=0; i < a.size(); i++) norm += std::fabs(a[i]);
  return norm;
}
double MATLIB_NAMESPACE normL2(const ShortArray& a) {
  double norm = 0.e0;
  for (unsigned int i=0; i < a.size(); i++) norm += a[i]*a[i];
  return std::sqrt(norm);
}

// compute max absolute value
double MATLIB_NAMESPACE normLInf(const ShortArray& a) {
  double norm = 0.e0;
  for (unsigned int i=0; i < a.size(); i++) {
    double test = std::fabs(a[i]);
    if (test > norm) norm = test;
  }
  return norm;
}

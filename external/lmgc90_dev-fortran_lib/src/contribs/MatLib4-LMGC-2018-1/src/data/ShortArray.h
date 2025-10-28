/*
 *  $Id: ShortArray.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_SHORT_ARRAY_H
#define ZORGLIB_DATA_SHORT_ARRAY_H

// config
#include <matlib_macros.h>

// std C library
#include <cstdlib>
// std C++ library
#include <iostream>
#include <stdexcept>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for expressions returning (short) arrays.
 */
class ShortArrayExpression {
  
 public:
  
  // constructor
  ShortArrayExpression() {}
  
  // copy constructor
  ShortArrayExpression(const ShortArrayExpression&) {}
  
  // destructor
  virtual ~ShortArrayExpression() {}
  
  // size
  virtual unsigned int size() const = 0;
  
  // expression operator
  virtual double operator[](unsigned int) const = 0;
  
  // print to string object
  std::string toString() const;
};

// define output stream operator
inline
std::ostream& operator<<(std::ostream& os,const ShortArrayExpression& obj) {
  os << obj.toString(); return os;
}


/**
 * Class for short arrays.
 */
class ShortArray {

 private:
  
  // size
  unsigned int sz;
  
  // pointer to data
  double *v;

  // data
  double *data;

 public:

  // constructor
  ShortArray(unsigned int = 0);

  // subarray constructor
  ShortArray(const ShortArray&,unsigned int,unsigned int = 0);
  ShortArray(const ShortArrayExpression&,unsigned int,unsigned int = 0);
  
  // wrapper constructor (for the experienced user)
  ShortArray(double*,unsigned int);

  // copy constructor
  ShortArray(const ShortArray&);
  ShortArray(const ShortArrayExpression&);
  
  // destructor
  virtual ~ShortArray() {
    if (data) delete [] data;
  }

  // assignment operator
  ShortArray& operator=(const ShortArray&) throw (std::range_error);
  ShortArray& operator=(const ShortArrayExpression&) throw (std::range_error);
  ShortArray& operator=(double);
  
  // unary operators
  ShortArray& operator+=(const ShortArray&) throw (std::range_error);
  ShortArray& operator+=(const ShortArrayExpression&) throw (std::range_error);
  ShortArray& operator-=(const ShortArray&) throw (std::range_error);
  ShortArray& operator-=(const ShortArrayExpression&) throw (std::range_error);
  ShortArray& operator*=(double);
  ShortArray& operator/=(double);
  
  // size
  unsigned int size() const {return sz;}
  
  // resize
  void resize(unsigned int);

  // wrap C array (for the experienced user)
  void wrap(double*,unsigned int);

  // access operators
  double operator[](unsigned int i) const {return v[i];}
  double& operator[](unsigned int i) {return v[i];}
  double operator()(unsigned int) const throw (std::out_of_range);
  double& operator()(unsigned int) throw (std::out_of_range);

  // print to string object
  std::string toString() const;
};

// define output stream operator
inline
std::ostream& operator<<(std::ostream& os,const ShortArray& obj) {
  os << obj.toString(); return os;
}

/**
 * ShortArray sums and differences.
 */
class ShortArraySum : public ShortArrayExpression {
  
 private:
  
  // pointers to arrays
  const ShortArray *a,*b;
  
 public:
  
  // constructor
  ShortArraySum(const ShortArray& aa,const ShortArray& bb) {
    if (aa.size() != bb.size()) 
      throw std::invalid_argument("ShortArraySum");
    a = &aa; b = &bb;
  }
  
  // copy constructor
  ShortArraySum(const ShortArraySum& src) {a = src.a; b = src.b;}
  
  // destructor
  ~ShortArraySum() {}
  
  // size
  unsigned int size() const {return (*a).size();}
  
  // expression operator
  double operator[](unsigned int i) const {return (*a)[i]+(*b)[i];}
};

class ShortArraySum1 : public ShortArrayExpression {
  
 private:
  
  // pointers to arrays
  const ShortArray *a;
  const ShortArrayExpression *b;
  
 public:
    
  // constructor
  ShortArraySum1(const ShortArray& aa,const ShortArrayExpression& bb) {
    if (aa.size() != bb.size()) 
      throw std::invalid_argument("ShortArraySum1");
    a = &aa; b = &bb;
  }
  
  // copy constructor
  ShortArraySum1(const ShortArraySum1& src) {a = src.a; b = src.b;}
  
  // destructor
  ~ShortArraySum1() {}
  
  // size
  unsigned int size() const {return (*a).size();}
  
  // expression operator
  double operator[](unsigned int i) const {return (*a)[i]+(*b)[i];}
};

class ShortArraySum2 : public ShortArrayExpression {
  
 private:
  
  // pointers to arrays
  const ShortArrayExpression *a,*b;
  
 public:
    
  // constructor
  ShortArraySum2(const ShortArrayExpression& aa,
                 const ShortArrayExpression& bb) {
    if (aa.size() != bb.size()) 
      throw std::invalid_argument("ShortArraySum2");
    a = &aa; b = &bb;
  }
  
  // copy constructor
  ShortArraySum2(const ShortArraySum2& src) {a = src.a; b = src.b;}
  
  // destructor
  ~ShortArraySum2() {}
  
  // size
  unsigned int size() const {return (*a).size();}
  
  // expression operator
  double operator[](unsigned int i) const {return (*a)[i]+(*b)[i];}
};

inline ShortArraySum operator+(const ShortArray& a,
                               const ShortArray& b) {
  return ShortArraySum(a,b);
}
inline ShortArraySum1 operator+(const ShortArray& a,
                                const ShortArrayExpression& b) {
  return ShortArraySum1(a,b);
}
inline ShortArraySum1 operator+(const ShortArrayExpression& a,
                                const ShortArray& b) {
  return ShortArraySum1(b,a);
}
inline ShortArraySum2 operator+(const ShortArrayExpression& a,
                                const ShortArrayExpression& b) {
  return ShortArraySum2(b,a);
}

class ShortArrayDifference : public ShortArrayExpression {
  
 private:
  
  // pointers to arrays
  const ShortArray *a,*b;
  
 public:
  
  // constructor
  ShortArrayDifference(const ShortArray& aa,const ShortArray& bb) {
    if (aa.size() != bb.size()) 
      throw std::invalid_argument("ShortArrayDifference");
    a = &aa; b = &bb;
  }
  
  // copy constructor
  ShortArrayDifference(const ShortArrayDifference& src) {a = src.a; b = src.b;}
  
  // destructor
  ~ShortArrayDifference() {}
  
  // size
  unsigned int size() const {return (*a).size();}
  
  // expression operator
  double operator[](unsigned int i) const {return (*a)[i]-(*b)[i];}
};

class ShortArrayDifference1 : public ShortArrayExpression {
  
 private:
  
  // pointers to arrays
  const ShortArray *a;
  const ShortArrayExpression *b;
  
 public:
    
  // constructor
  ShortArrayDifference1(const ShortArray& aa,
                        const ShortArrayExpression& bb) {
    if (aa.size() != bb.size()) 
      throw std::invalid_argument("ShortArrayDifference1");
    a = &aa; b = &bb;
  }
  
  // copy constructor
  ShortArrayDifference1(const ShortArrayDifference1& src) {a = src.a; b = src.b;}
  
  // destructor
  ~ShortArrayDifference1() {}
  
  // size
  unsigned int size() const {return (*a).size();}
  
  // expression operator
  double operator[](unsigned int i) const {return (*a)[i]-(*b)[i];}
};

class ShortArrayDifference2 : public ShortArrayExpression {
  
 private:
  
  // pointers to arrays
  const ShortArrayExpression *a;
  const ShortArray *b;
  
 public:
    
  // constructor
  ShortArrayDifference2(const ShortArrayExpression& aa,
                        const ShortArray& bb) {
    if (aa.size() != bb.size()) 
      throw std::invalid_argument("ShortArrayDifference1");
    a = &aa; b = &bb;
  }
  
  // copy constructor
  ShortArrayDifference2(const ShortArrayDifference2& src) {a = src.a; b = src.b;}
  
  // destructor
  ~ShortArrayDifference2() {}
  
  // size
  unsigned int size() const {return (*a).size();}
  
  // expression operator
  double operator[](unsigned int i) const {return (*a)[i]-(*b)[i];}
};

class ShortArrayDifference3 : public ShortArrayExpression {
  
 private:
  
  // pointers to arrays
  const ShortArrayExpression *a,*b;
  
 public:
    
  // constructor
  ShortArrayDifference3(const ShortArrayExpression& aa,
                        const ShortArrayExpression& bb) {
    if (aa.size() != bb.size()) 
      throw std::invalid_argument("ShortArrayDifference2");
    a = &aa; b = &bb;
  }
  
  // copy constructor
  ShortArrayDifference3(const ShortArrayDifference3& src) {a = src.a; b = src.b;}
  
  // destructor
  ~ShortArrayDifference3() {}
  
  // size
  unsigned int size() const {return (*a).size();}
  
  // expression operator
  double operator[](unsigned int i) const {return (*a)[i]-(*b)[i];}
};

inline ShortArrayDifference operator-(const ShortArray& a,
                                      const ShortArray& b) {
  return ShortArrayDifference(a,b);
}
inline ShortArrayDifference1 operator-(const ShortArray& a,
                                       const ShortArrayExpression& b) {
  return ShortArrayDifference1(a,b);
}
inline ShortArrayDifference2 operator-(const ShortArrayExpression& a,
                                       const ShortArray& b) {
  return ShortArrayDifference2(a,b);
}
inline ShortArrayDifference3 operator-(const ShortArrayExpression& a,
                                       const ShortArrayExpression& b) {
  return ShortArrayDifference3(a,b);
}


/**
 * Array products.
 */
class ShortArrayScalarProduct : public ShortArrayExpression {
  
 private:
  
  // pointer to Array
  const ShortArray *a;
  double fact;
  
 public:
    
  // constructor
  ShortArrayScalarProduct(const ShortArray& aa,double f) {
    a = &aa; fact = f;
  }
  
  // copy constructor
  ShortArrayScalarProduct(const ShortArrayScalarProduct& src) {
    a = src.a; fact = src.fact;
  }
  
  // destructor
  ~ShortArrayScalarProduct() {}
  
  // size
  unsigned int size() const {return (*a).size();}
  
  // expression operator
  double operator[](unsigned int i) const {return fact*(*a)[i];}
};

class ShortArrayScalarProduct1 : public ShortArrayExpression {
  
 private:
  
  // pointer to Array
  const ShortArrayExpression *a;
  double fact;
  
 public:
    
  // constructor
  ShortArrayScalarProduct1(const ShortArrayExpression& aa,double f) {
    a = &aa; fact = f;
  }
  
  // copy constructor
  ShortArrayScalarProduct1(const ShortArrayScalarProduct1& src) {
    a = src.a; fact = src.fact;
  }
  
  // destructor
  ~ShortArrayScalarProduct1() {}
  
  // size
  unsigned int size() const {return (*a).size();}
  
  // expression operator
  double operator[](unsigned int i) const {return fact*(*a)[i];}
};

inline ShortArrayScalarProduct operator*(double f,const ShortArray& a) {
  return ShortArrayScalarProduct(a,f);
}
inline ShortArrayScalarProduct1 operator*(double f,
                                          const ShortArrayExpression& a) {
  return ShortArrayScalarProduct1(a,f);
}

inline ShortArrayScalarProduct operator/(const ShortArray& a,double f) {
  return ShortArrayScalarProduct(a,1.0/f);
}
inline ShortArrayScalarProduct1 operator/(const ShortArrayExpression& a,
                                          double f) {
  return ShortArrayScalarProduct1(a,1.0/f);
}

inline ShortArrayScalarProduct operator-(const ShortArray& a) {
  return ShortArrayScalarProduct(a,-1);
}
inline ShortArrayScalarProduct1 operator-(const ShortArrayExpression& a) {
  return ShortArrayScalarProduct1(a,-1);
}

// define inner product
double innerProd(const ShortArray&,const ShortArray&) throw (std::range_error);

// compute norm of an array
double normL1(const ShortArray&);
double normL2(const ShortArray&);

// compute max absolute value
double normLInf(const ShortArray&);

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif


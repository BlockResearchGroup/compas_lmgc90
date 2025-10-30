/*
 *  $Id: ShortMatrix.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_SHORT_MATRIX_H
#define ZORGLIB_DATA_SHORT_MATRIX_H

// config
#include <matlib_macros.h>

// std C library
#include <cstdlib>
#include <cstring>
// std C++ library
#include <stdexcept>
// local
#ifndef WITH_MATLIB_H
#include <data/ShortArray.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for expressions returning (short) matrices.
 */
class ShortMatrixExpression {
  
 public:
  
  // constructor
  ShortMatrixExpression() {}
  
  // copy constructor
  ShortMatrixExpression(const ShortMatrixExpression&) {}
  
  // destructor
  virtual ~ShortMatrixExpression() {}
  
  // matrix size
  virtual unsigned int nCols() const = 0;
  virtual unsigned int nRows() const = 0;
  
  // expression operator
  virtual double operator()(unsigned int,unsigned int) const = 0;
  
  // print to string object
  std::string toString() const;
};

// define output stream operator
inline
std::ostream& operator<<(std::ostream& os,const ShortMatrixExpression& obj) {
  os << obj.toString(); return os;
}


/**
 * Class for small, full matrices.
 */
class ShortMatrix {

 private:
  
  // size
  unsigned int nr,nc;
  
  // data
  double *data;

  // pointers to rows
  double* *r;

 public:
  
  // constructor
  ShortMatrix(unsigned int = 0,unsigned int = 1);
  
  // submatrix constructor
  ShortMatrix(const ShortMatrix&,unsigned int,unsigned int,
              unsigned int = 0,unsigned int = 0);
  ShortMatrix(const ShortMatrixExpression&,unsigned int,unsigned int,
              unsigned int = 0,unsigned int = 0);
    
  // wrapper constructor (for the experienced user)
  ShortMatrix(double*,unsigned int,unsigned int);

  // copy constructor
  ShortMatrix(const ShortMatrix&);
  ShortMatrix(const ShortMatrixExpression&);
  
  // destructor
  virtual ~ShortMatrix() {
    if (r) delete [] r;
    if (data) delete [] data;
  }
  
  // assignment operator
  ShortMatrix& operator=(const ShortMatrix&) throw (std::range_error);
  ShortMatrix& operator=(const ShortMatrixExpression&) throw (std::range_error);
  ShortMatrix& operator=(double);

  // unary operators
  ShortMatrix& operator+=(const ShortMatrix&) throw (std::range_error);
  ShortMatrix& operator+=(const ShortMatrixExpression&) throw (std::range_error);
  ShortMatrix& operator-=(const ShortMatrix&) throw (std::range_error);
  ShortMatrix& operator-=(const ShortMatrixExpression&) throw (std::range_error);
  ShortMatrix& operator*=(double);
  ShortMatrix& operator/=(double);
  
  // size
  unsigned int nRows() const {return nr;}
  unsigned int nCols() const {return nc;}
  
  // resize
  void resize(unsigned int,unsigned int);
  
  // wrap C array (for the experienced user)
  void wrap(double*,unsigned int,unsigned int);

  // access operators
  double* operator[](unsigned int i) const {return r[i];}
  double operator()(unsigned int,unsigned int) const throw (std::out_of_range);
  double& operator()(unsigned int,unsigned int) throw (std::out_of_range);
  
  // print to string object
  std::string toString() const;
  
  // build matrix by vector outer product
  static void outerProd(const ShortArray&,const ShortArray&,ShortMatrix&);
};

// define output stream operator
inline
std::ostream& operator<<(std::ostream& os,const ShortMatrix& obj) {
  os << obj.toString(); return os;
}


/**
 * ShortMatrix sums and differences.
 */
class ShortMatrixSum : public ShortMatrixExpression {
  
 private:
  
  // pointers to matrices
  const ShortMatrix *A,*B;
  
 public:
  
  // constructor
  ShortMatrixSum(const ShortMatrix& a,const ShortMatrix& b) {
    if ((a.nCols() != b.nCols()) || (a.nRows() != b.nRows()))
      throw std::invalid_argument("ShortMatrixSum");
    A = &a; B = &b;
  }
  
  // copy constructor
  ShortMatrixSum(const ShortMatrixSum& src) {A = src.A; B = src.B;}
  
  // destructor
  ~ShortMatrixSum() {}
  
  // matrix size
  unsigned int nCols() const {return A->nCols();}
  unsigned int nRows() const {return A->nRows();}
  
  // expression operator
  double operator()(unsigned int i,unsigned int j) const {return (*A)[i][j]+(*B)[i][j];}
};

class ShortMatrixSum1 : public ShortMatrixExpression {
  
 private:
  
  // pointers to matrices
  const ShortMatrix *A;
  const ShortMatrixExpression *B;
  
 public:
    
  // constructor
  ShortMatrixSum1(const ShortMatrix& a,const ShortMatrixExpression& b) {
    if ((a.nCols() != b.nCols()) || (a.nRows() != b.nRows()))
      throw std::invalid_argument("ShortMatrixSum1");
    A = &a; B = &b;
  }
  
  // copy constructor
  ShortMatrixSum1(const ShortMatrixSum1& src) {A = src.A; B = src.B;}
  
  // destructor
  ~ShortMatrixSum1() {}
  
  // matrix size
  unsigned int nCols() const {return A->nCols();}
  unsigned int nRows() const {return A->nRows();}
  
  // expression operator
  double operator()(unsigned int i,unsigned int j) const {return (*A)[i][j]+(*B)(i,j);}
};

class ShortMatrixSum2 : public ShortMatrixExpression {
  
 private:
  
  // pointers to matrices
  const ShortMatrixExpression *A,*B;
  
 public:
    
  // constructor
  ShortMatrixSum2(const ShortMatrixExpression& a,
                  const ShortMatrixExpression& b) {
    if ((a.nCols() != b.nCols()) || (a.nRows() != b.nRows()))
      throw std::invalid_argument("ShortMatrixSum2");
    A = &a; B = &b;
  }
  
  // copy constructor
  ShortMatrixSum2(const ShortMatrixSum2& src) {A = src.A; B = src.B;}
  
  // destructor
  ~ShortMatrixSum2() {}
  
  // matrix size
  unsigned int nCols() const {return A->nCols();}
  unsigned int nRows() const {return A->nRows();}
  
  // expression operator
  double operator()(unsigned int i,unsigned int j) const {return (*A)(i,j)+(*B)(i,j);}
};

inline ShortMatrixSum operator+(const ShortMatrix& A,
                                const ShortMatrix& B) {
  return ShortMatrixSum(A,B);
}
inline ShortMatrixSum1 operator+(const ShortMatrix& A,
                                 const ShortMatrixExpression& B) {
  return ShortMatrixSum1(A,B);
}
inline ShortMatrixSum1 operator+(const ShortMatrixExpression& A,
                                 const ShortMatrix& B) {
  return ShortMatrixSum1(B,A);
}
inline ShortMatrixSum2 operator+(const ShortMatrixExpression& A,
                                 const ShortMatrixExpression& B) {
  return ShortMatrixSum2(A,B);
}

class ShortMatrixDifference : public ShortMatrixExpression {
  
 private:
  
  // pointers to matrices
  const ShortMatrix *A,*B;
  
 public:
  
  // constructor
  ShortMatrixDifference(const ShortMatrix& a,const ShortMatrix& b) {
    if ((a.nCols() != b.nCols()) || (a.nRows() != b.nRows()))
      throw std::invalid_argument("ShortMatrixDifference");
    A = &a; B = &b;
  }
  
  // copy constructor
  ShortMatrixDifference(const ShortMatrixDifference& src) {A = src.A; B = src.B;}
  
  // destructor
  ~ShortMatrixDifference() {}
  
  // matrix size
  unsigned int nCols() const {return A->nCols();}
  unsigned int nRows() const {return A->nRows();}
  
  // expression operator
  double operator()(unsigned int i,unsigned int j) const {return (*A)[i][j]-(*B)[i][j];}
};

class ShortMatrixDifference1 : public ShortMatrixExpression {
  
 private:
  
  // pointers to matrices
  const ShortMatrix *A;
  const ShortMatrixExpression *B;
  
 public:
    
  // constructor
  ShortMatrixDifference1(const ShortMatrix& a,
                         const ShortMatrixExpression& b) {
    if ((a.nCols() != b.nCols()) || (a.nRows() != b.nRows()))
      throw std::invalid_argument("ShortMatrixDifference1");
    A = &a; B = &b;
  }
  
  // copy constructor
  ShortMatrixDifference1(const ShortMatrixDifference1& src) {A = src.A; B = src.B;}
  
  // destructor
  ~ShortMatrixDifference1() {}
  
  // matrix size
  unsigned int nCols() const {return A->nCols();}
  unsigned int nRows() const {return A->nRows();}
  
  // expression operator
  double operator()(unsigned int i,unsigned int j) const {return (*A)[i][j]-(*B)(i,j);}
};

class ShortMatrixDifference2 : public ShortMatrixExpression {
  
 private:
  
  // pointers to matrices
  const ShortMatrixExpression *A;
  const ShortMatrix *B;
  
 public:
    
  // constructor
  ShortMatrixDifference2(const ShortMatrixExpression& a,
                         const ShortMatrix& b) {
    if ((a.nCols() != b.nCols()) || (a.nRows() != b.nRows()))
      throw std::invalid_argument("ShortMatrixDifference1");
    A = &a; B = &b;
  }
  
  // copy constructor
  ShortMatrixDifference2(const ShortMatrixDifference2& src) {A = src.A; B = src.B;}
  
  // destructor
  ~ShortMatrixDifference2() {}
  
  // matrix size
  unsigned int nCols() const {return A->nCols();}
  unsigned int nRows() const {return A->nRows();}
  
  // expression operator
  double operator()(unsigned int i,unsigned int j) const {return (*A)(i,j)-(*B)[i][j];}
};

class ShortMatrixDifference3 : public ShortMatrixExpression {
  
 private:
  
  // pointers to matrices
  const ShortMatrixExpression *A,*B;
  
 public:
  
  // constructor
  ShortMatrixDifference3(const ShortMatrixExpression& a,
                         const ShortMatrixExpression& b) {
    if ((a.nCols() != b.nCols()) || (a.nRows() != b.nRows()))
      throw std::invalid_argument("ShortMatrixDifference2");
    A = &a; B = &b;
  }
  
  // copy constructor
  ShortMatrixDifference3(const ShortMatrixDifference3& src) {A = src.A; B = src.B;}
  
  // destructor
  ~ShortMatrixDifference3() {}
  
  // matrix size
  unsigned int nCols() const {return A->nCols();}
  unsigned int nRows() const {return A->nRows();}
  
  // expression operator
  double operator()(unsigned int i,unsigned int j) const {return (*A)(i,j)-(*B)(i,j);}
};

inline ShortMatrixDifference operator-(const ShortMatrix& A,
                                       const ShortMatrix& B) {
  return ShortMatrixDifference(A,B);
}
inline ShortMatrixDifference1 operator-(const ShortMatrix& A,
                                        const ShortMatrixExpression& B) {
  return ShortMatrixDifference1(A,B);
}
inline ShortMatrixDifference2 operator-(const ShortMatrixExpression& A,
                                        const ShortMatrix& B) {
  return ShortMatrixDifference2(A,B);
}
inline ShortMatrixDifference3 operator-(const ShortMatrixExpression& A,
                                        const ShortMatrixExpression& B) {
  return ShortMatrixDifference3(A,B);
}


/**
 * Matrix products.
 */
class ShortMatrixScalarProduct : public ShortMatrixExpression {
  
 private:
  
  // pointers to matrices
  const ShortMatrix *A;
  double fact;
  
 public:
    
  // constructor
  ShortMatrixScalarProduct(const ShortMatrix& a,double f) {A = &a; fact = f;}
  
  // copy constructor
  ShortMatrixScalarProduct(const ShortMatrixScalarProduct& src) {
    A = src.A; fact = src.fact;
  }
  
  // destructor
  ~ShortMatrixScalarProduct() {}
  
  // matrix size
  unsigned int nCols() const {return A->nCols();}
  unsigned int nRows() const {return A->nRows();}
  
  // expression operator
  double operator()(unsigned int i,unsigned int j) const {return fact*(*A)[i][j];}
};

class ShortMatrixScalarProduct1 : public ShortMatrixExpression {
  
 private:
  
  // pointers to matrices
  const ShortMatrixExpression *A;
  double fact;
  
 public:
    
  // constructor
  ShortMatrixScalarProduct1(const ShortMatrixExpression& a,double f) {
    A = &a; fact = f;
  }
  
  // copy constructor
  ShortMatrixScalarProduct1(const ShortMatrixScalarProduct1& src) {
    A = src.A; fact = src.fact;
  }
  
  // destructor
  ~ShortMatrixScalarProduct1() {}
  
  // matrix size
  unsigned int nCols() const {return A->nCols();}
  unsigned int nRows() const {return A->nRows();}
  
  // expression operator
  double operator()(unsigned int i,unsigned int j) const {return fact*(*A)(i,j);}
};

inline ShortMatrixScalarProduct operator*(double f,const ShortMatrix& a) {
  return ShortMatrixScalarProduct(a,f);
}
inline ShortMatrixScalarProduct1 operator*(double f,
                                           const ShortMatrixExpression& a) {
  return ShortMatrixScalarProduct1(a,f);
}

inline ShortMatrixScalarProduct operator/(const ShortMatrix& a,double f) {
  return ShortMatrixScalarProduct(a,1.0/f);
}
inline ShortMatrixScalarProduct1 operator/(const ShortMatrixExpression& a,
                                           double f) {
  return ShortMatrixScalarProduct1(a,1.0/f);
}

inline ShortMatrixScalarProduct operator-(const ShortMatrix& a) {
  return ShortMatrixScalarProduct(a,-1);
}
inline ShortMatrixScalarProduct1 operator-(const ShortMatrixExpression& a) {
  return ShortMatrixScalarProduct1(a,-1);
}

// define outer product
class ShortArrayOuterProduct : public ShortMatrixExpression {
  
 private:
  
  // pointers to arrays
  const ShortArray *a,*b;
  
 public:
  
  // constructor
  ShortArrayOuterProduct(const ShortArray& aa,const ShortArray& bb) {
    a = &aa; b = &bb;
  }
  
  // copy constructor
  ShortArrayOuterProduct(const ShortArrayOuterProduct& src) {a = src.a; b = src.b;}
  
  // destructor
  ~ShortArrayOuterProduct() {}
  
  // matrix size
  unsigned int nCols() const {return b->size();}
  unsigned int nRows() const {return a->size();}
  
  // expression operator
  double operator()(unsigned int i,unsigned int j) const {return (*a)[i]*(*b)[j];}
};
inline
ShortArrayOuterProduct outerProd(const ShortArray& a,const ShortArray& b) {
  return ShortArrayOuterProduct(a,b);
}

// simple matrix*array product
ShortArray operator*(const ShortMatrix&,const ShortArray&) throw (std::range_error);

// simple matrix*matrix product
ShortMatrix operator*(const ShortMatrix&,const ShortMatrix&) throw (std::range_error);

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif



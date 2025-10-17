/*
 *  $Id: ShortMatrix.cpp 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "ShortMatrix.h"

// std C library
#include <cstring>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for ShortMatrixExpression.
 */

// print to string object
std::string ShortMatrixExpression::toString() const {
  O_STRING_STREAM os;
  os << std::endl;
  for (unsigned int i=0; i < this->nRows(); i++) {
    os << "|";
    for (unsigned int j=0; j < this->nCols(); j++) os << " " << (*this)(i,j);
    os << "|" << std::endl;
  }
#ifdef HAVE_SSTREAM
  return os.str();
#else
  return std::string(os.str(),os.pcount());
#endif
}


/*
 * Methods for ShortMatrix.
 */

// constructor
ShortMatrix::ShortMatrix(unsigned int m,unsigned int n) {
  nr = m; nc = n;
  if (m > 0 && n > 0) {
    unsigned int memSize = m*n;
    data = new double[memSize];
    r = new double*[m];
    double* p = data;
    for (unsigned int i=0; i < m; i++, p+=n) r[i] = p;
  }
  else {
    data = 0;
    r = 0;
  }
}

// submatrix constructor
ShortMatrix::ShortMatrix(const ShortMatrix& src,unsigned int m,unsigned int n,
                         unsigned int idx0,unsigned int jdx0) {
  nr = m; nc = n;
  if (m > 0 && n > 0) {
    r = new double*[m];
    for (unsigned int i=0; i < m; i++) r[i] = src.r[idx0+i]+jdx0;
  }
  else
    r = 0;
  data = 0;
}
ShortMatrix::ShortMatrix(const ShortMatrixExpression& src,unsigned int m,unsigned int n,
                         unsigned int idx0,unsigned int jdx0) {
  nr = m; nc = n;
  if (m > 0 && n > 0) {
    r = new double*[m];
    unsigned int memSize = m*n;
    data = new double[memSize];
    double* p = data;
    unsigned int ii=idx0;
    for (unsigned int i=0; i < nr; i++, ii++) {
      r[i] = p;
      unsigned int jj=jdx0;
      for (unsigned int j=0; j < nc; j++, jj++, p++) (*p) = src(ii,jj);
    }
  }
  else {
    data = 0;
    r = 0;
  }
}
  
// wrapper constructor (for the experienced user)
ShortMatrix::ShortMatrix(double* a,unsigned int m,unsigned int n) {
  nr = m;
  nc = n;
  if (m > 0 && n > 0) {
    r = new double*[m];
    double* p = a;
    for (unsigned int i=0; i < nr; i++, p+=nc)
      r[i] = p;
  }
  data = 0;
}

// copy constructor
ShortMatrix::ShortMatrix(const ShortMatrix& src) {
  nr = src.nr; nc = src.nc;
  if (nr > 0 && nc > 0) {
    r = new double*[nr];
    unsigned int memSize = nr*nc;
    data = new double[memSize];
    std::memcpy(data,src.data,memSize*sizeof(double));
    double* p = data;
    for (unsigned int i=0; i < nr; i++, p+=nc) r[i] = p;
  }
  else {
    data = 0;
    r = 0;
  }
}
ShortMatrix::ShortMatrix(const ShortMatrixExpression& src) {
  nr = src.nRows(); nc = src.nCols();
  if (nr > 0 && nc > 0) {
    r = new double*[nr];
    unsigned int memSize = nr*nc;
    data = new double[memSize];
    double* p = data;
    for (unsigned int i=0; i < nr; i++) {
      r[i] = p;
      for (unsigned int j=0; j < nc; j++, p++) (*p) = src(i,j);
    }
  }
  else {
    data = 0;
    r = 0;
  }
}

// assignment operator
ShortMatrix& ShortMatrix::operator=(const ShortMatrix& src) throw (std::range_error) {
  if (nRows() != src.nRows() || nCols() != src.nCols()) 
    throw std::range_error("ShortMatrix");
  for (unsigned int i=0; i < src.nRows(); i++)
    std::memcpy(r[i],src.r[i],src.nCols()*sizeof(double));
  return *this;
}
ShortMatrix& ShortMatrix::operator=(const ShortMatrixExpression& src)
 throw (std::range_error) {
  if (nRows() != src.nRows() || nCols() != src.nCols())
    throw std::range_error("ShortMatrix");
  for (unsigned int i=0; i < src.nRows(); i++) {
    double* p = r[i];
    for (unsigned int j=0; j < src.nCols(); j++, p++) (*p) = src(i,j);
  }
  return *this;
}
ShortMatrix& ShortMatrix::operator=(double val) {
  for (unsigned int i=0; i < nRows(); i++) {
    double* p = r[i];
    for (unsigned int j=0; j < nCols(); j++, p++) (*p) = val;
  }
  return *this;
}

// unary operators
ShortMatrix& ShortMatrix::operator+=(const ShortMatrix& src) throw (std::range_error) {
  if (nRows() != src.nRows() || nCols() != src.nCols()) 
    throw std::range_error("ShortMatrix");
  for (unsigned int i=0; i < src.nRows(); i++) {
    double* p = r[i];
    double* q = src.r[i];
    for (unsigned int j=0; j < src.nCols(); j++, p++, q++) (*p) += (*q);
  }
  return *this;
}
ShortMatrix& ShortMatrix::operator+=(const ShortMatrixExpression& src)
 throw (std::range_error) {
  if (nRows() != src.nRows() || nCols() != src.nCols())
    throw std::range_error("ShortMatrix");
  for (unsigned int i=0; i < src.nRows(); i++) {
    double* p = r[i];
    for (unsigned int j=0; j < src.nCols(); j++, p++) (*p) += src(i,j);
  }
  return *this;
}
ShortMatrix& ShortMatrix::operator-=(const ShortMatrix& src) throw (std::range_error) {
  if (nRows() != src.nRows() || nCols() != src.nCols()) 
    throw std::range_error("ShortMatrix");
  for (unsigned int i=0; i < src.nRows(); i++) {
    double* p = r[i];
    double* q = src.r[i];
    for (unsigned int j=0; j < src.nCols(); j++, p++, q++) (*p) -= (*q);
  }
  return *this;
}
ShortMatrix& ShortMatrix::operator-=(const ShortMatrixExpression& src)
 throw (std::range_error) {
  if (nRows() != src.nRows() || nCols() != src.nCols())
    throw std::range_error("ShortMatrix");
  for (unsigned int i=0; i < src.nRows(); i++) {
    double* p = r[i];
    for (unsigned int j=0; j < src.nCols(); j++, p++) (*p) -= src(i,j);
  }
  return *this;
}
ShortMatrix& ShortMatrix::operator*=(double val) {
  for (unsigned int i=0; i < nRows(); i++) {
    double* p = r[i];
    for (unsigned int j=0; j < nCols(); j++, p++) (*p) *= val;
  }
  return *this;
}
ShortMatrix& ShortMatrix::operator/=(double val) {
  double valInv = 1.e0/val;
  for (unsigned int i=0; i < nRows(); i++) {
    double* p = r[i];
    for (unsigned int j=0; j < nCols(); j++, p++) (*p) *= valInv;
  }
  return *this;
}

// resize
void ShortMatrix::resize(unsigned int m,unsigned int n) {
  if (nRows() == m && nCols() == n) return;
  if (data) delete [] data;
  if (r) delete [] r;
  nr = m;
  nc = n;
  unsigned int memSize = nr*nc;
  if (memSize > 0) {
    data = new double[memSize];
    r = new double*[m];
    double* p = data;
    for (unsigned int i=0; i < m; i++, p+=n) r[i] = p;
  } else {
    data = 0;
    r = 0;
  }
}

// wrap C array (for the experienced user)
void ShortMatrix::wrap(double* a,unsigned int m,unsigned int n) {
  if (data) delete [] data;
  nr = m;
  nc = n;
  if (m > 0 && n > 0) {
    r = new double*[m];
    double* p = a;
    for (unsigned int i=0; i < nr; i++, p+=nc)
      r[i] = p;
  }
  data = 0;
}

// access operators
double ShortMatrix::operator()(unsigned int i,unsigned int j) const throw (std::out_of_range) {
  if (i < nRows() && j < nCols())
    return r[i][j];
  else
    throw std::out_of_range("ShortMatrix");
}
double& ShortMatrix::operator()(unsigned int i,unsigned int j) throw (std::out_of_range) {
  if (i < nRows() && j < nCols())
    return r[i][j];
  else
    throw std::out_of_range("ShortMatrix");
}

// print to string object
std::string ShortMatrix::toString() const {
  O_STRING_STREAM os;
  os << std::endl;
  for (unsigned int i=0; i < nRows(); i++) {
    os << "|";
    for (unsigned int j=0; j < nCols(); j++) os << " " << r[i][j];
    os << "|" << std::endl;
  }
#ifdef HAVE_SSTREAM
  return os.str();
#else
  return std::string(os.str(),os.pcount());
#endif
}

// build matrix by vector outer product
void ShortMatrix::outerProd(const ShortArray& a,const ShortArray& b,
                            ShortMatrix& M) {
  for (unsigned int i=0; i < a.size(); i++)
    for (unsigned int j=0; j < b.size(); j++) M.r[i][j] = a[i]*b[j];
}

// simple matrix*array product
ShortArray MATLIB_NAMESPACE operator*(const ShortMatrix& A,const ShortArray& b)
 throw (std::range_error) {
  if (A.nCols() != b.size()) throw std::range_error("matrix*array product");
  ShortArray c(A.nRows());
  for (unsigned int i=0; i < A.nRows(); i++) {
    double val = 0.0e0;
    for (unsigned int j=0; j < A.nCols(); j++) val += A[i][j]*b[j];
    c[i] = val;
  }
  return c;
}

// simple matrix*matrix product
ShortMatrix MATLIB_NAMESPACE operator*(const ShortMatrix& A,const ShortMatrix& B)
 throw (std::range_error) {
  if (A.nCols() != B.nRows()) throw std::range_error("matrix*matrix product");
   ShortMatrix C(A.nRows(),B.nCols());
   for (unsigned int i=0; i < A.nRows(); i++) 
     for (unsigned int j=0; j < B.nCols(); j++) {
       double val = 0.0e0;
       for (unsigned int k=0; k < A.nCols(); k++) val += A[i][k]*B[k][j];
       C[i][j] = val;
     }
   return C;
 }

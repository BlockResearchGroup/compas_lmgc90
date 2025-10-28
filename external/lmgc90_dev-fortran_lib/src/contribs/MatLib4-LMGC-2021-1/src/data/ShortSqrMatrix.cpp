/*
 *  $Id: ShortSqrMatrix.cpp 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "ShortSqrMatrix.h"

// std C library
#include <cmath>
#include <cstring>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for ShortSqrMatrix
 */

// assignment operator
ShortSqrMatrix& ShortSqrMatrix::operator=(const ShortMatrix& src)
 throw (std::range_error) {
  if ((size() != src.nRows()) || (size() != src.nCols()))
    throw std::range_error("ShortSqrMatrix");
  for (unsigned int i=0; i < src.nRows(); i++)
    std::memcpy((*this)[i],src[i],src.nCols()*sizeof(double));
  return *this;
}
ShortSqrMatrix& ShortSqrMatrix::operator=(const ShortMatrixExpression& src)
 throw (std::range_error) {
  if ((size() != src.nRows()) || (size() != src.nCols())) 
    throw std::range_error("ShortSqrMatrix");
  for (unsigned int i=0; i < src.nRows(); i++) {
    double* p = (*this)[i];
    for (unsigned int j=0; j < src.nCols(); j++, p++) (*p) = src(i,j);
  }
  return *this;
}
ShortSqrMatrix& ShortSqrMatrix::operator=(double val) {
  for (unsigned int i=0; i < size(); i++) {
    double* p = (*this)[i];
    for (unsigned int j=0; j < size(); j++, p++) (*p) = val;
  }
  return *this;
}

// get identity matrix
ShortSqrMatrix ShortSqrMatrix::identity(unsigned int m) {
  ShortSqrMatrix I(m);
  for (unsigned int i=0; i < m; i++) {
    std::memset(I[i],0,m*sizeof(double));
    I[i][i] = 1;
  }
  return I;
}

// invert (in place)
void ShortSqrMatrix::invert() throw (SingularMatrixException) {
  
  /*
   * Matrix inversion by Gauss-Jordan elimination.
   */
  piv.resize(size());
  unsigned int i,j,k;
  for (k=0; k < size(); k++) piv[k] = k;

  // loop on columns
  for (k=0; k < size(); k++) {
    
    // search for pivot
    double xpiv = std::fabs((*this)[k][k]);
    for (j=k+1; j < size(); j++) {
      double test = std::fabs((*this)[k][j]);
      if (test <= xpiv) continue;
      xpiv = test;
      piv[k] = j;
    }
    
    // swap columns
    if (piv[k] > k) {
      for (i=0; i < size(); i++) {
        double tmp = (*this)[i][k];
        (*this)[i][k] = (*this)[i][piv[k]];
        (*this)[i][piv[k]] = tmp;
      }
    }
    
    // check for singularity
    if (xpiv < 1.e-16)
      throw SingularMatrixException("in ShortSqrMatrix::invert()");
    double val = 1.e0/(*this)[k][k];
    
    (*this)[k][k] = 1.e0;
    for (j=0; j < size(); j++) (*this)[k][j] *= val;
    
    // loop on rows
    for (i=0; i < size(); i++) {
      if (i == k) continue;
      double coef = (*this)[i][k];
      (*this)[i][k] = 0.e0;
      for (j=0; j < size(); j++) (*this)[i][j] -= (*this)[k][j]*coef;
    }
  }
  
  // swap rows
  k = size()-1;
  do {
    if (piv[k] > k) {
      for (j=0; j < size(); j++) {
        double tmp = (*this)[piv[k]][j];
        (*this)[piv[k]][j] = (*this)[k][j];
        (*this)[k][j] = tmp;
      }
    }
  } while (k-- != 0);
}

// factorize (LU)
void ShortSqrMatrix::factorize(bool s)
 throw (SingularMatrixException) {

  // initialize
  sym = s;
  piv.resize(size());
  unsigned int i,j,k;
  for (k=0; k < size(); k++) piv[k] = k;
   
  // SYMMETRIC CASE
  if (sym) {
    
    // factorization (U^t*D*U decomposition)
    for (k=0; k < size()-1; k++) {
      
      // search for pivot
      double xpiv = std::fabs((*this)[k][k]);
      for (i=k+1; i < size(); i++) {
        double test = std::fabs((*this)[i][i]);
        if (test <= xpiv) continue;
        xpiv = test;
        piv[k] = i;
      }
      
      // swap rows and columns
      if (piv[k] > k) {
        unsigned int idx = piv[k];
        double tmp;
        for (j=0; j < k; j++) {
          tmp = (*this)[j][k];
          (*this)[j][k] = (*this)[j][idx];
          (*this)[j][idx] = tmp;
        }
        tmp = (*this)[k][k];
        (*this)[k][k] = (*this)[idx][idx];
        (*this)[idx][idx] = tmp;
        for (j=k+1; j < idx; j++) {
          tmp = (*this)[k][j];
          (*this)[k][j] = (*this)[j][idx];
          (*this)[j][idx] = tmp;
        }
        for (j=idx+1; j < size(); j++) {
          tmp = (*this)[k][j];
          (*this)[k][j] = (*this)[idx][j];
          (*this)[idx][j] = tmp;
        }
      }
      
      // check for singularity
      if (xpiv < 1.e-16)
        throw SingularMatrixException("in ShortSqrMatrix::factorize()");
      double val = 1.e0/(*this)[k][k];
      
      // A(k,k+1:n) := A(k,k+1:n)/A(k,k)
      for (j=k+1; j < size(); j++) (*this)[k][j] *= val;
      
      // A(k+1:n,k+1:n) := A(k+1:n,k+1:n)-A(k,k+1:n)*A(k,k)*A(k,k+1:n)
      for (i=k+1; i < size(); i++)
        for (j=i; j < size(); j++) 
          (*this)[i][j] -= (*this)[k][i]*(*this)[k][k]*(*this)[k][j];
    }
  }
  else {

    // LU factorization
    for (k=0; k < size()-1; k++) {

      // search for pivot
      double xpiv = std::fabs((*this)[k][k]);
      for (i=k+1; i < size(); i++) {
        double test = std::fabs((*this)[i][k]);
        if (test <= xpiv) continue;
        xpiv = test;
        piv[k] = i;
      }

      // swap rows
      if (piv[k] > k) {
        for (j=0; j < size(); j++) {
          double tmp = (*this)[k][j];
          (*this)[k][j] = (*this)[piv[k]][j];
          (*this)[piv[k]][j] = tmp;
        }
      }

      // check for singularity
      if (xpiv < 1.e-16)
        throw SingularMatrixException("in ShortSqrMatrix::factorize()");
      double val = 1.e0/(*this)[k][k];

      // A(k,k+1:n) := A(k,k+1:n)/A(k,k)
      for (j=k+1; j < size(); j++) (*this)[k][j] *= val;

      // A(k+1:n,k+1:n) := A(k+1:n,k+1:n)-A(k+1:n,k)*A(k,k+1:n)
      for (i=k+1; i < size(); i++) 
        for (j=k+1; j < size(); j++) 
          (*this)[i][j] -= (*this)[i][k]*(*this)[k][j];
    }
  }

  return;
}

// back-substitute
void ShortSqrMatrix::backsubstitute(ShortArray& x,const ShortArray& b) const {

  // Step 0: prepare for permutations of b
  unsigned int i,j;
  for (i=0; i < size(); i++) x[i] = b[i];
  
  // SYMMETRIC CASE
  if (sym) {

    // Step 1: U^t*z=b
    for (i=0; i < size(); i++) {
      unsigned int idx = piv[i];
      double sum = x[idx];
      if (idx > i) x[idx] = x[i];
      for (j=0; j < i; j++) sum -= (*this)[j][i]*x[j];
      x[i] = sum;
    }
  
    // Step 2: D*y=z
    for (i=0; i < size(); i++) x[i] /= (*this)[i][i];
  
    // Step 3: U*x=y
    i = size()-1;
    do {
      for (j=i+1; j < size(); j++) x[i] -= (*this)[i][j]*x[j];
    } while (i-- != 0);
  
    // Step 4: permutations of x (we did double pivoting in factorize)
    i = size()-2;
    do {
      if (piv[i] == i) continue;
      unsigned int idx = piv[i];
      double tmp = x[i];
      x[i] = x[idx];
      x[idx] = tmp;
    } while (i-- != 0);
  }
  else {

    // Step 1: L*z=b
    for (i=0; i < size(); i++) {
      unsigned int idx = piv[i];
      double sum = x[idx];
      if (idx > i) x[idx] = x[i];
      for (j=0; j < i; j++) sum -= (*this)[i][j]*x[j];
      x[i] = sum/(*this)[i][i];
    }
  
    // Step 2: U*x=z
    i = size()-1;
    do {
      for (j=i+1; j < size(); j++) x[i] -= (*this)[i][j]*x[j];
    } while (i-- != 0);
  }
  
  return;
}

// solve linear system
void ShortSqrMatrix::solve(ShortArray& x,const ShortArray& b,bool sym)
 throw (SingularMatrixException) {
  factorize(sym);
  backsubstitute(x,b);
}

/*
 * Solve symmetric linear system A*x=b
 * by Cholesky factorization of A, stored in the upper triangular.
 * A diagonal term mu2 is added to A if it is not
 * positive definite (threshold = mu1).
 * This version includes a map of active variables.
 */
bool ShortSqrMatrix::symSolve(ShortArray& x,const ShortArray& b,
                              std::vector<bool>& map,
                              double mu1,double mu2) {
   
  // check arguments
  if (x.size() != size() || b.size() != size())
    throw std::range_error("ShortSqrMatrix::symSolve()");
   
  // Cholesky factorization (U^t*U decomposition)
  unsigned int i,j,k;
  bool modified = false;
  for (i=0; i < size(); i++) {
    if (!map[i]) continue;
    for (j=i; j < size(); j++) {
      if (!map[j]) continue;

      // A(i,i:n) := A(i,i:n) - A(1:i-1,i) A(1:i-1,i:n)
      double sum = (*this)[i][j];
      for (k=0; k < i; k++) {
        if (!map[k]) continue;
        sum -= (*this)[k][i]*(*this)[k][j];
      }

      if (j == i) {
        if (sum > mu1)
          (*this)[i][j] = std::sqrt(sum);
        else {
          (*this)[i][j] = mu2;
          modified = true;
        }
      }
      else
        (*this)[i][j] = sum/(*this)[i][i];
    }
  }

  // backsubstitution

  // Step 1: U^t*z=b
  for (i=0; i < size(); i++) {
    if (!map[i]) continue;

    double sum = b[i];
    for (k=0; k < i; k++) {
      if (!map[k]) continue;
      sum -= (*this)[k][i]*x[k];
    }
    x[i] = sum/(*this)[i][i];
  }

  // Step 2: U*x=z
  i = size()-1;
  do {
    if (!map[i]) continue;

    double sum = x[i];
    for (k=i+1; k < size(); k++) {
      if (!map[k]) continue;
      sum -= (*this)[i][k]*x[k];
    }
    x[i] = sum/(*this)[i][i];
  } while (i-- != 0);

  return modified;
}

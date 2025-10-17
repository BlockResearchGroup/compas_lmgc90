/*
 *  $Id: ShortSqrMatrix.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_SHORT_SQUARE_MATRIX_H
#define ZORGLIB_DATA_SHORT_SQUARE_MATRIX_H

// config
#include <matlib_macros.h>

// STL
#include <vector>
// local
#ifndef WITH_MATLIB_H
#include <data/Exceptions.h>
#include <data/ShortArray.h>
#include <data/ShortMatrix.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Exception thrown when attempting to factorize a singular matrix.
 */
class SingularMatrixException : public ZException {
  
 public:
  
  // default constructor
  SingularMatrixException(const std::string& msg = "singular matrix")
  : ZException(msg) {}
  
  // copy constructor
  SingularMatrixException(const SingularMatrixException& src)
  : ZException(src) {}
};


/**
 * Class for small, full, square matrices.
 */
class ShortSqrMatrix : virtual public ShortMatrix {

 private:

  // pivot indices
  std::vector<unsigned int> piv;

  // symmetric factorization
  bool sym;

 public:

  // constructor
  ShortSqrMatrix(unsigned int m = 0) : ShortMatrix(m,m) {}
  
  // submatrix constructor
  ShortSqrMatrix(const ShortSqrMatrix& src,unsigned int m,unsigned int idx0 = 0) 
   : ShortMatrix(src,m,m,idx0,idx0) {}
  ShortSqrMatrix(const ShortMatrixExpression& src,unsigned int m,unsigned int idx0 = 0) 
   : ShortMatrix(src,m,m,idx0,idx0) {}
      
  // wrapper constructor (for the experienced user)
  ShortSqrMatrix(double* a,unsigned int m)
   : ShortMatrix(a,m,m) {}

  // copy constructor
  ShortSqrMatrix(const ShortSqrMatrix& src) 
   : ShortMatrix(src) {piv = src.piv; sym = src.sym;}
                         
  // destructor
  virtual ~ShortSqrMatrix() {}
                         
  // assignment operator
  ShortSqrMatrix& operator=(const ShortMatrix&)
    throw (std::range_error);
  ShortSqrMatrix& operator=(const ShortMatrixExpression&)
    throw (std::range_error);
  ShortSqrMatrix& operator=(double);
  
  // size
  unsigned int size() const {return nRows();}
  
  // resize
  void resize(unsigned int m) {ShortMatrix::resize(m,m);}
    
  // wrap C array (for the experienced user)
  void wrap(double* a,unsigned int m) {ShortMatrix::wrap(a,m,m);}

  // get identity matrix
  static ShortSqrMatrix identity(unsigned int);
  
  // invert (in place)
  void invert() throw (SingularMatrixException);

  // factorize (LU)
  void factorize(bool) throw (SingularMatrixException);

  // back-substitute
  void backsubstitute(ShortArray&,const ShortArray&) const;
  
  // solve linear system
  void solve(ShortArray&,const ShortArray&,bool=false)
    throw (SingularMatrixException);
  
  // solve a linear system known to be symmetric
  // (will be modified if not definite positive)
  bool symSolve(ShortArray&,const ShortArray&,std::vector<bool>&,
                double = 0.e0,double = 0.e0);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

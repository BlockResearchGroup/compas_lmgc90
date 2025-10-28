/*
 *  $Id: TabulatedFunction.h 198 2016-02-26 10:46:57Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_TABULATED_FUNCTION_H
#define ZORGLIB_DATA_TABULATED_FUNCTION_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include <data/Function.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Tabulated functions.
 */
class TabulatedFunction : virtual public Function {
  
 protected:
  
  // number of points
  unsigned int nPts;
  
  // pointer to last index
  unsigned int lastIdx;

  // list of points
  double *x,*y;
  
 public:
    
  // default constructor
  TabulatedFunction(const std::string& = "no name",unsigned int = 0);
  
  // constructor
  TabulatedFunction(const std::string&,unsigned int,double*,double*);
  
  // copy constructor
  TabulatedFunction(const TabulatedFunction&);
  
  // destructor
  virtual ~TabulatedFunction();
  
  // duplicate object
  virtual TabulatedFunction* clone() const {return new TabulatedFunction(*this);}

  // resize
  void resize(unsigned int);
  
  // get number of points
  unsigned int nPoints() const {return nPts;}
  
  // get point of given index
  std::pair<double,double> getPoint(unsigned int i) const {
    return std::pair<double,double>(x[i],y[i]);
  }

  // set value
  void setPoint(unsigned int,double,double);
  
  // get value
  double value(double);
  
  // get derivative
  double slope(double);
  
  // get value and derivative
  double value(double,double&);
  
  // print-out
  std::string toString() const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

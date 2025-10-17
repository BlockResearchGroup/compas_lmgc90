/*
 *  $Id: TabulatedFunction2.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_TABULATED_FUNCTION2_H
#define ZORGLIB_DATA_TABULATED_FUNCTION2_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include <data/Function2.h>
#include <data/TabulatedFunction.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Tabulated functions (with C1-continuity).
 */
class TabulatedFunction2 : virtual public TabulatedFunction,
                           virtual public Function2 {

 protected:
  
  // list of slopes
  double *dy;

  // slope initialization flag
  bool updateSlopes;

  // compute slopes
  void computeSlopes();

 public:

  // default constructor
  TabulatedFunction2(const std::string& = "no name",unsigned int = 0);

  // constructor
  TabulatedFunction2(const std::string&,unsigned int,double*,double*);

  // copy constructor
  TabulatedFunction2(const TabulatedFunction2&);

  // destructor
  virtual ~TabulatedFunction2();

  // duplicate object
  virtual TabulatedFunction2* clone() const {return new TabulatedFunction2(*this);}

  // resize
  void resize(unsigned int);
  
  // set value
  void setPoint(unsigned int,double,double);
  
  // get value
  double value(double);
  
  // get derivative
  double slope(double);
  
  // get curvature
  double curvature(double);
  
  // get value and derivative
  double value(double,double&);
  
  // get value and derivatives
  double value(double,double&,double&);
  
  // print-out
  std::string toString() const {return TabulatedFunction::toString();}
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

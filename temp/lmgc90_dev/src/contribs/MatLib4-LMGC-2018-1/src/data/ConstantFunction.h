/*
 *  $Id: ConstantFunction.h 231 2017-03-16 21:15:11Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_CONSTANT_FUNCTION_H
#define ZORGLIB_DATA_CONSTANT_FUNCTION_H

// config
#include <matlib_macros.h>

// std C library
#include <cstring>

// local
#ifndef WITH_MATLIB_H
#include <data/Function2.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Constant functions.
 */
class ConstantFunction : virtual public Function2 {
  
 protected:
  
  // value
  double val;
  
 public:
    
  // default constructor
  ConstantFunction(const std::string& s = "no name",double v = 0.0)
  : Function(s) {val = v;}
  
  // copy constructor
  ConstantFunction(const ConstantFunction& src)
  : Function(src) {val = src.val;}
  
  // destructor
  virtual ~ConstantFunction() {}
  
  // duplicate object
  virtual ConstantFunction* clone() const {return new ConstantFunction(*this);}
  
  // get value
  double value(double) {return val;}
  
  // get derivative
  double slope(double) {return 0.0;}
  
  // get curvature
  double curvature(double) {return 0.0;}

  // get value and derivative
  double value(double,double&) {return val;}
  
  // get value and derivatives
  double value(double,double&,double&) {return val;}

  // print-out
  std::string toString() const {
    O_STRING_STREAM os;
    os << "Constant function: " << getName() << " with value " << val << std::endl;
#ifdef HAVE_SSTREAM
    return os.str();
#else
    return std::string(os.str(),os.pcount());
#endif
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

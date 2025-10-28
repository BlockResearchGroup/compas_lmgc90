/*
 *  $Id: Function.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_FUNCTION_H
#define ZORGLIB_DATA_FUNCTION_H

// config
#include <matlib_macros.h>

// std C++ library
#include <string>
// local
#ifndef WITH_MATLIB_H
#include <data/Cloneable.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for functions.
 */
class Function : virtual public Cloneable {
  
 protected:
  
  // name of function
  std::string name;
  
 public:
  
  // constructor
  Function(const std::string& s = "no name") {name = s;}
  
  // copy constructor
  Function(const Function& src) {name = src.name;}
  
  // destructor
  virtual ~Function() {}
  
  // duplicate object
  virtual Function* clone() const = 0;

  // get the function's name
  std::string getName() const {return name;}
  
  // get value
  virtual double value(double) = 0;
  
  // get derivative
  virtual double slope(double) = 0;
  
  // get value and derivative
  virtual double value(double,double&) = 0;
  
  // print-out
  virtual std::string toString() const = 0;
};


/**
 * Overload the output stream operator
 */
inline std::ostream& operator<<(std::ostream& os,const Function& obj) {
  os << obj.toString(); return os;
}

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

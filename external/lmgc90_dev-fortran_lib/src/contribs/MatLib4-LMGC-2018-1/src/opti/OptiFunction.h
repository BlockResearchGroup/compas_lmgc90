/*
 *  $Id: OptiFunction.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_OPTI_FUNCTION_H
#define ZORGLIB_OPTI_FUNCTION_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include <data/Cloneable.h>
#include <data/ShortArray.h>
#include <data/ShortSqrMatrix.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Generic class (interface) for objective functions and constraints.
 */
class OptiFunction : virtual public Cloneable {
 
 protected:
  
  // empty constructor
  OptiFunction() {}
  
  // copy constructor
  OptiFunction(const OptiFunction&) {}
  
 public:
  
  // destructor
  virtual ~OptiFunction() {}
  
  // clone operation
  virtual OptiFunction* clone() const = 0;
  
  // get dimension of definition space
  virtual unsigned int dimension() const = 0;
  
  // get value and derivatives
  virtual double value(const ShortArray&,
                       ShortArray* = 0,bool = false,
                       ShortSqrMatrix* = 0,bool = false) const = 0;
  double value(const ShortArray& x,ShortArray& g) const {
    return value(x,&g,true);
  }
  double value(const ShortArray& x,ShortArray& g,ShortSqrMatrix& H) const {
    return value(x,&g,true,&H,true);
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif


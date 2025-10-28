/*
 *  $Id: Function2.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_FUNCTION2_H
#define ZORGLIB_DATA_FUNCTION2_H

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
 * Base class for functions for which a curvature can be computed.
 * It normally implies C1 continuity.
 */
class Function2 : virtual public Function {

 public:
  
  // destructor
  virtual ~Function2() {}
  
  // duplicate object
  virtual Function2* clone() const = 0;
  
  // get value
  virtual double value(double) = 0;
  
  // get derivative
  virtual double slope(double) = 0;
  
  // get curvature
  virtual double curvature(double) = 0;
  
  // get value and derivative
  virtual double value(double,double&) = 0;
  
  // get value and derivatives
  virtual double value(double,double&,double&) = 0;
};
  
#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif
  
#endif  

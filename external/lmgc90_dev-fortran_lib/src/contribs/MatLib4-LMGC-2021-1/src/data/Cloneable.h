/*
 *  $Id: Cloneable.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_CLONEABLE_H
#define ZORGLIB_DATA_CLONEABLE_H

// config
#include <matlib_macros.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Interface for cloneable objects.
 */
class Cloneable {
  
 public:
  
  // virtual destructor
  virtual ~Cloneable() {}

  // clone operation
  virtual Cloneable* clone() const = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
